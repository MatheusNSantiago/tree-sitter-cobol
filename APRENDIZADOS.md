# Aprendizados - Otimização do Parser COBOL

Este arquivo documenta experimentos e aprendizados durante a otimização do parser.

---

## 2026-01-23: END-* Opcional Causa Explosão Massiva de Estados

### Experimento
Tentei unificar as regras `_*_block` e `_*_sentence` em uma única regra com END-* opcional:

```javascript
// ANTES (block + sentence separados)
add_statement: ($) => choice($._add_block, $._add_sentence),
_add_block: ($) => seq(..., $._END_ADD),
_add_sentence: ($) => seq(...),

// TENTATIVA (END opcional)
add_statement: ($) => seq(..., op($._END_ADD)),
```

### Resultado
- **DESASTRE**: Compilação não terminou após 4+ minutos
- Uso de RAM: 30GB+
- O parser não conseguiu ser gerado

### Por quê?
Quando END-* é opcional na mesma regra, o parser não sabe onde o statement termina. Isso cria:
1. Ambiguidade massiva em cada ponto onde um statement pode terminar
2. O parser precisa considerar TODAS as combinações possíveis
3. Explosão exponencial de estados

### Conclusão
**A separação block/sentence é uma SOLUÇÃO, não o problema.** Ela permite que o parser faça escolhas determinísticas mais cedo:
- Se vê END-*, sabe que é block
- Se não vê END-*, sabe que é sentence

**NUNCA tente unificar block/sentence com END opcional.**

---

## 2026-01-23: Scanner-based Body Detection - Tentativa Falha

### Experimento
Tentei usar o scanner para detectar quando o corpo de um statement termina, emitindo um token zero-width `_IF_BODY_CONTINUES` que seria verdadeiro enquanto não houvesse terminador (END-IF, ELSE, período).

```javascript
// Gramática proposta
_if_body: ($) =>
  repeat(seq(
    $._IF_BODY_CONTINUES,  // Scanner detecta se há mais statements
    $._statement,
  )),

if_statement: ($) =>
  seq(
    $._IF, $.expr,
    $._if_body,
    optional($._END_IF),  // ← AINDA OPCIONAL - PROBLEMA!
  ),
```

### Resultado
- tree-sitter generate travou por muito tempo (não terminou)
- O `optional($._END_IF)` ainda causava explosão
- O scanner não resolvia o problema fundamental

### Por quê?
O erro foi manter `optional($._END_IF)` na gramática. O scanner apenas detectava se havia mais statements, mas **não decidia** se era block ou sentence.

Para funcionar, o scanner deveria:
1. **CONSUMIR** o END-IF se existir
2. Retornar um token `_IF_END` que representa "fim do IF" (explícito ou implícito)
3. A gramática NÃO teria `optional` - teria um token obrigatório que o scanner sempre fornece

```javascript
// CORRETO: Scanner decide, gramática não tem optional
if_statement: ($) =>
  seq(
    $._IF, $.expr,
    $._if_body,
    $._IF_END,  // Scanner: consome END-IF ou retorna implicit-end
  ),
```

### Conclusão
**Scanner-based approach pode funcionar, MAS:**
1. O scanner deve CONSUMIR o terminador, não apenas detectá-lo
2. A gramática não pode ter `optional($._END_*)` - isso causa explosão
3. O scanner deve retornar tokens diferentes ou o mesmo token para block/sentence
4. Complexidade alta - requer reescrever scanner e gramática juntos

**Para implementar corretamente no futuro:**
- Scanner mantém stack de statements abertos
- Ao ver END-*, verifica se corresponde ao topo da stack
- Se sim, consome e retorna token de fim
- Se não há END-*, retorna token de fim implícito
- Gramática usa token obrigatório, não optional

---

## 2026-01-23: Scanner-based IF_END - Segunda Tentativa (Parcialmente Funcional)

### Experimento
Implementei o scanner que:
1. Consome END-IF quando encontra
2. Retorna IF_END zero-width quando vê período (fim implícito)
3. Retorna false em outros casos para deixar parser tentar outras opções

Gramática unificada:
```javascript
if_statement: ($) =>
  seq(
    $._IF, $.expr, $.statement_block,
    optional(seq($._ELSE, $.statement_block)),
    $._IF_END,  // Scanner decide: consome END-IF ou retorna implícito
  ),
```

### Resultado
- **STATE_COUNT: 26313** (redução de -2844 estados, -9.7%)
- **Sucesso: 58%** dos arquivos parsearam corretamente
- **Falha: 42%** dos arquivos com erros

### Problema Identificado
Quando o scanner é chamado buscando IF_END:
1. Se retorna `false`, a função toda retorna
2. Outros tokens válidos (como PREFIX para números de sequência) nunca são verificados
3. Os números de sequência (cols 0-6) são parseados como integers em vez de PREFIX

### Por que 58% funcionou?
Arquivos sem números de sequência nas colunas 1-6 funcionaram.
Arquivos COM números de sequência falharam porque PREFIX não era reconhecido dentro do IF body.

### Tentativa de Correção: Mover IF_END para o final
Movi IF_END para ser verificado APÓS todos os outros tokens. Resultado: piorou para 0.78% de sucesso.

O problema é que IF_END precisa ser verificado cedo o suficiente para consumir END-IF, mas tarde o suficiente para não bloquear outros tokens.

### Insights para Implementação Futura
1. **Ordem importa**: O scanner verifica tokens na ordem do código. Se IF_END retorna false e a função retorna, tokens subsequentes não são verificados.

2. **Não usar return false cedo**: Se um token não casa, não retornar false imediatamente. Continuar verificando outros tokens.

3. **Contexto é necessário**: O scanner precisa saber se está em contexto de IF body para decidir corretamente.

4. **Possível solução**: Usar estado no scanner para tracking:
```c
typedef struct {
  int if_depth;  // Quantos IFs estão abertos
  // ...
} Scanner;
```
E só retornar IF_END quando `if_depth > 0` E vê END-IF ou período.

5. **Redução promissora**: -2844 estados (-9.7%) mostra que a abordagem tem potencial real. Vale investir mais tempo para fazer funcionar.

---

## Template para Novos Aprendizados

```markdown
## YYYY-MM-DD: Título do Aprendizado

### Experimento
O que foi tentado.

### Resultado
O que aconteceu (números, métricas).

### Por quê?
Explicação técnica.

### Conclusão
O que aprendemos e regras a seguir.
```
