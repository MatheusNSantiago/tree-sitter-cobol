# Tree-sitter COBOL - Otimização de Parser

## Objetivo

Reduzir a complexidade do parser (STATE_COUNT e LARGE_STATE_COUNT) **sem perder funcionalidade**. O parser atual está muito grande (~50MB, 32k estados) e precisa ser otimizado.

## Métricas Atuais (Baseline)

```
STATE_COUNT:       32861
LARGE_STATE_COUNT: 5452
SYMBOL_COUNT:      854
Parser size:       50MB
Arquivos de teste: 386 (100% sucesso)
```

## Problema Principal Identificado: STATEMENT/STATEMENT-END

Em COBOL, o `END-*` é **opcional** para a maioria dos statements. Por exemplo:
```cobol
IF X = 1
    MOVE Y TO Z
END-IF.          *> END-IF é opcional

IF X = 1
    MOVE Y TO Z.  *> Válido sem END-IF
```

Isso cria explosão combinatória quando statements são aninhados (IF dentro de PERFORM dentro de EVALUATE). Cada nível adiciona um fator de 2x nas combinações de estados.

### Abordagem atual (tentativa, pode não ser a melhor)
Separação em dois tipos de regras:
- `STATEMENT_block` - com END obrigatório
- `STATEMENT_sentence` - sem END

**NOTA:** Esta abordagem ajudou a reduzir estados, mas provavelmente não é a solução ótima. Outras alternativas devem ser exploradas:
- Scanner-based END detection
- Precedência dinâmica (`prec.dynamic`)
- "Bag of clauses" pattern
- Delimitadores no scanner (similar a Ruby/TypeScript)

---

## Técnicas de Otimização (Pesquisa Consolidada)

### Fontes Principais
- [Tips and Tricks for a grammar author - tree-sitter Wiki](https://github.com/tree-sitter/tree-sitter/wiki/Tips-and-Tricks-for-a-grammar-author)
- [The Grammar DSL - tree-sitter docs](https://tree-sitter.github.io/tree-sitter/creating-parsers/2-the-grammar-dsl.html)
- [Profiling a Parser - Discussion #994](https://github.com/tree-sitter/tree-sitter/discussions/994)
- [Parser is too large/bloated - Issue #56 tree-sitter-blade](https://github.com/EmranMR/tree-sitter-blade/issues/56)
- [Large parser.c in tree-sitter-sql - Issue #1799](https://github.com/tree-sitter/tree-sitter/issues/1799)
- [tree-sitter grammar for Nim - Discussion #2248](https://github.com/tree-sitter/tree-sitter/discussions/2248)
- [The IELR(1) algorithm - ScienceDirect](https://www.sciencedirect.com/science/article/pii/S0167642309001191)

---

## 1. Diagnóstico: Identificar Regras Problemáticas

### Comando principal
```bash
tree-sitter generate --report-states-for-rule - 2>&1 | head -50
```

> "This shows you all the rules and their possible # of states, the biggest ones *can* be but are not always an indicator of a poorly written rule."
> — [tree-sitter Wiki](https://github.com/tree-sitter/tree-sitter/wiki/Tips-and-Tricks-for-a-grammar-author)

### Interpretação
- **Regras no topo** = alvos prioritários para refatoração
- **`binary_expression` alto** = normal (muitos operadores)
- **`statement` ou `clause` alto** = problema estrutural

---

## 2. Técnica: Flatten Optional Sequences (MAIOR IMPACTO)

### O problema
```javascript
// ANTI-PATTERN: Explosão combinatória
rule: $ => seq(
  'X',
  optional(choice($.a, $.b, $.c)),
  optional(choice($.d, $.e)),
  optional(choice($.f, $.g, $.h)),
  'Y'
)
// Gera até 3 × 2 × 3 = 18 caminhos de estados
```

### A solução
```javascript
// PATTERN: Linearização
rule: $ => seq(
  'X',
  repeat(choice($.a, $.b, $.c, $.d, $.e, $.f, $.g, $.h)),
  'Y'
)
// Um único loop com 8 transições
```

> "We're down 400 in state count, almost a 20% reduction, from just two simple refactors"
> — [tree-sitter Wiki](https://github.com/tree-sitter/tree-sitter/wiki/Tips-and-Tricks-for-a-grammar-author)

### Impacto esperado: 10-30% redução

### Aplicabilidade COBOL
**Extremamente alta** — COBOL statements têm muitas cláusulas opcionais:
```cobol
READ file-name NEXT RECORD INTO identifier KEY IS key-name INVALID KEY ...
```

---

## 3. Técnica: Separar Nonterminals Muito Permissivos

### O problema
```javascript
// ANTI-PATTERN: _expression usado em todo lugar
condition: $ => $._expression
arithmetic: $ => $._expression
identifier_ref: $ => $._expression
```

### A solução
```javascript
// PATTERN: Nonterminals específicos por contexto
_condition: $ => $._boolean_expression
_arithmetic: $ => $._numeric_expression
_identifier_ref: $ => $.variable
```

> "you use `_expression` very gratuitously… Maybe separate… into a `_pure_expression`?"
> — [Discussion #994](https://github.com/tree-sitter/tree-sitter/discussions/994)

### Impacto esperado: Variável, pode ser dramático

---

## 4. Técnica: External Scanner para Complexidade Contextual

### O princípio
> "The Externals API… good for things like indentation based delimiters and automatic semicolon insertion."
> — [Hacker News - tree-sitter maintainer](https://news.ycombinator.com/item?id=18213022)

### Candidatos COBOL para scanner externo
1. **PICTURE strings** (`PIC S9(9)V99 COMP-3`)
2. **Área A / Área B** (colunas 8-11 vs 12-72)
3. **Continuation lines** (hífen na coluna 7)

### Impacto esperado
> "Reducing the number of external literals have a great impact on the parser size... cut parser size from 132MiB to 59MiB."
> — [tree-sitter-nim Discussion #2248](https://github.com/tree-sitter/tree-sitter/discussions/2248)

---

## 5. Técnica: Uso Correto de `inline`

### Quando AJUDA
```javascript
// Regras simples usadas em poucos lugares
inline: $ => [$._punctuation, $._small_clause]
```

### Quando PREJUDICA
```javascript
// Regras complexas usadas em muitos lugares
// NÃO inline: $._statement, $._expression
```

> "inline: automatically removed… replacing all of their usages with a copy of their definition."
> — [Grammar DSL docs](https://tree-sitter.github.io/tree-sitter/creating-parsers/2-the-grammar-dsl.html)

> "start inlining arbitrary symbols, this sometimes improves or worsens the parser size."
> — [Discussion #994](https://github.com/tree-sitter/tree-sitter/discussions/994)

### Impacto esperado: Imprevisível - SEMPRE medir antes/depois

---

## 6. Técnica: Precedência para Eliminar Conflitos

### Princípio
> "prec: used to resolve LR(1) Conflicts at parser-generation time."
> — [Grammar DSL docs](https://tree-sitter.github.io/tree-sitter/creating-parsers/2-the-grammar-dsl.html)

```javascript
binary_expression: $ => choice(
  prec.left(1, seq($._expr, '+', $._expr)),
  prec.left(2, seq($._expr, '*', $._expr)),
)
```

### Evitar `conflicts` quando possível
> "conflicts: Tree-sitter will use the GLR algorithm to explore all the possible interpretations."
> — [Grammar DSL docs](https://tree-sitter.github.io/tree-sitter/creating-parsers/2-the-grammar-dsl.html)

Cada entrada em `conflicts` é permissão para explosão de estados.

---

## 7. Técnica: Keyword Extraction com `word`

```javascript
word: $ => $.identifier
```

> "By default, Tree-sitter would recognize `instanceofSomething` as two separate tokens…"
> — [Writing the Grammar docs](https://tree-sitter.github.io/tree-sitter/creating-parsers/3-writing-the-grammar.html)

### Aplicabilidade COBOL
Média-alta. COBOL tem muitas palavras reservadas e identificadores com hífen.

---

## 8. Case Studies Relevantes

| Gramática | Problema | Solução | Resultado |
|-----------|----------|---------|-----------|
| **SQL** | 83MB parser.c | Remover `select_statement` | 29MB (-65%) |
| **Nim** | 132MB | Minimizar external literals | 42MB (-68%) |
| **C** | `for_statement` 318 estados | Extrair `_for_statement_body` | -20% estados |

> "Removed select_statement: 29M parser.c" (from 83M original)
> — [Issue #1799](https://github.com/tree-sitter/tree-sitter/issues/1799)

---

## Estrutura do Projeto

```
tree-sitter-cobol/
├── grammar.js              # Gramática principal
├── src/
│   ├── grammar/
│   │   ├── base.js         # Regras básicas (variable, WORD, etc)
│   │   ├── expression.js   # Expressões e comparadores
│   │   ├── keywords.js     # Keywords COBOL
│   │   ├── division/       # Divisões COBOL
│   │   └── statements/     # Statements (FOCO DA OTIMIZAÇÃO)
│   │       ├── index.js    # _statement, statement_block, etc
│   │       └── ...
│   ├── scanner.c           # Scanner externo
│   └── parser.c            # Parser gerado (NÃO EDITAR)
├── test/
│   └── corpus/             # 386 arquivos COBOL para teste
├── scripts/
│   └── benchmark.sh        # Script de benchmark
├── benchmark-baseline.json # Métricas baseline
└── benchmark-report.json   # Último relatório
```

---

## Processo de Iteração

```
┌─────────────────────────────────────────────────────────────────┐
│  1. Rodar: tree-sitter generate --report-states-for-rule -      │
│  2. Identificar top 5 regras com mais estados                   │
│  3. Escolher UMA regra para refatorar                           │
│  4. Aplicar UMA técnica (flatten, split, extract)               │
│  5. Rodar: tree-sitter generate                                 │
│  6. Verificar: --report-states-for-rule (a regra melhorou?)     │
│  7. Rodar: ./scripts/benchmark.sh 100                           │
│  8. Validar: todos os 386 arquivos ainda passam?                │
│  9. Se MELHOROU (menos estados + 100% testes) → git commit      │
│  10. Se PIOROU ou QUEBROU → git checkout . (descartar)          │
│  11. Repetir                                                    │
└─────────────────────────────────────────────────────────────────┘
```

### Regra de Commit
**Só commitar quando houver melhoria real:**
- STATE_COUNT diminuiu E
- 100% dos arquivos em `test/corpus/` ainda passam

Não commitar mudanças que pioram ou que não têm impacto.

> "Refactor, regenerate, check state counts, rinse and repeat."
> — [tree-sitter Wiki](https://github.com/tree-sitter/tree-sitter/wiki/Tips-and-Tricks-for-a-grammar-author)

---

## Validação Obrigatória

```bash
# 1. Verificar métricas
./scripts/benchmark.sh 100

# 2. Verificar TODOS os arquivos do corpus
FAIL=0
for f in test/corpus/*.cbl; do
    if tree-sitter parse "$f" 2>&1 | grep -q "ERROR\|MISSING"; then
        echo "FALHOU: $f"
        FAIL=1
    fi
done
[ $FAIL -eq 0 ] && echo "TODOS OS TESTES PASSARAM"
```

---

## Critérios de Sucesso

Uma iteração é bem-sucedida se:

1. ✅ `STATE_COUNT` diminuiu (qualquer redução)
2. ✅ `LARGE_STATE_COUNT` não aumentou significativamente
3. ✅ **100% dos arquivos em `test/corpus/` parseiam sem erros**

---

## Comandos Úteis

```bash
# Gerar parser
tree-sitter generate

# Ver estados por regra (PRINCIPAL FERRAMENTA)
tree-sitter generate --report-states-for-rule - 2>&1 | head -50

# Parsear um arquivo específico
tree-sitter parse test/corpus/ARQUIVO.cbl

# Benchmark completo
./scripts/benchmark.sh 100

# Testar todos os arquivos do corpus
for f in test/corpus/*.cbl; do
    tree-sitter parse "$f" 2>&1 | grep -q "ERROR" && echo "FAIL: $f"
done

# Ver métricas no parser.c
grep -E "STATE_COUNT|LARGE_STATE" src/parser.c | head -5

# Comparar antes/depois
git diff src/parser.c | grep -E "STATE_COUNT|LARGE_STATE"
```

---

## Histórico de Otimizações

| Data | Mudança | STATE antes | STATE depois | Diff |
|------|---------|-------------|--------------|------|
| 2026-01-22 | Baseline inicial | - | 32861 | - |

---

## Alvos Prioritários para Otimização

Baseado no `--report-states-for-rule`:

| Regra | Estados | Estratégia Sugerida |
|-------|---------|---------------------|
| `variable` | ~4600 | Simplificar qualificação IN/OF |
| `reference_modification` | ~4600 | Mover para scanner? |
| `_WORD_repeat1` | ~3600 | Avaliar necessidade |
| `subscript_reference` | ~2900 | Limitar aninhamento |
| `expr` | ~1900 | Separar bool/arith/data |
| `statement` relacionados | ~500+ | Flatten optionals |

---

## Notas Importantes

1. **Não edite `src/parser.c`** - é gerado automaticamente
2. **Uma mudança por vez** - facilita identificar o que funcionou
3. **`variable` é usada em centenas de lugares** - mudanças têm grande cascata
4. **O problema STATEMENT/END-STATEMENT** é provavelmente o maior contribuidor
5. **SQL embarcado tem impacto ínfimo** - não é alvo de otimização
