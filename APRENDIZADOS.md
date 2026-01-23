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
