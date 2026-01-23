#!/bin/bash
# Benchmark script para tree-sitter-cobol
# Usa os arquivos do corpus local (test/corpus/)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
CORPUS_DIR="$PROJECT_DIR/test/corpus"

cd "$PROJECT_DIR"

echo "========================================="
echo "  Tree-sitter COBOL Benchmark"
echo "========================================="

# 1. Gerar parser
echo -e "\n[1/4] Gerando parser..."
tree-sitter generate 2>&1 | tail -1 || {
    echo "Erro ao gerar parser"
    exit 1
}
echo "  ✓ Parser gerado"

# 2. Extrair métricas
echo -e "\n[2/4] Extraindo métricas..."

STATE_COUNT=$(grep -m1 "#define STATE_COUNT" src/parser.c | awk '{print $3}')
LARGE_STATE_COUNT=$(grep -m1 "#define LARGE_STATE_COUNT" src/parser.c | awk '{print $3}')
SYMBOL_COUNT=$(grep -m1 "#define SYMBOL_COUNT" src/parser.c | awk '{print $3}')
PARSER_SIZE=$(stat -c%s src/parser.c)

echo "  STATE_COUNT:       $STATE_COUNT"
echo "  LARGE_STATE_COUNT: $LARGE_STATE_COUNT"
echo "  SYMBOL_COUNT:      $SYMBOL_COUNT"
echo "  Parser size:       $(numfmt --to=iec $PARSER_SIZE)"

# 3. Parsear TODOS os arquivos do corpus
echo -e "\n[3/4] Parseando arquivos do corpus..."

TOTAL_FILES=$(ls "$CORPUS_DIR"/*.cbl 2>/dev/null | wc -l)
SUCCESS=0
FAILED=0
TOTAL_ERRORS=0
FAILED_LIST=""

for f in "$CORPUS_DIR"/*.cbl; do
    OUTPUT=$(tree-sitter parse "$f" 2>&1 || true)
    ERROR_COUNT=$(echo "$OUTPUT" | grep -c "ERROR\|MISSING") || ERROR_COUNT=0

    if [[ $ERROR_COUNT -eq 0 ]]; then
        SUCCESS=$((SUCCESS + 1))
    else
        FAILED=$((FAILED + 1))
        TOTAL_ERRORS=$((TOTAL_ERRORS + ERROR_COUNT))
        FAILED_LIST="$FAILED_LIST $(basename "$f")"
    fi
done

TOTAL=$((SUCCESS + FAILED))
if [[ $TOTAL -gt 0 ]]; then
    SUCCESS_RATE=$(awk "BEGIN {printf \"%.2f\", $SUCCESS * 100 / $TOTAL}")
    AVG_ERRORS=$(awk "BEGIN {printf \"%.1f\", $TOTAL_ERRORS / $TOTAL}")
else
    SUCCESS_RATE="0.00"
    AVG_ERRORS="0.0"
fi

echo "  Success: $SUCCESS / $TOTAL (${SUCCESS_RATE}%)"
echo "  Failed:  $FAILED (avg errors: $AVG_ERRORS)"

if [[ $FAILED -gt 0 ]]; then
    echo "  Arquivos com falha:$FAILED_LIST"
fi

# 4. Relatório JSON
echo -e "\n[4/4] Salvando relatório..."

cat > "$PROJECT_DIR/benchmark-report.json" << EOF
{
  "timestamp": "$(date -Iseconds)",
  "metrics": {
    "state_count": $STATE_COUNT,
    "large_state_count": $LARGE_STATE_COUNT,
    "symbol_count": $SYMBOL_COUNT,
    "parser_size_bytes": $PARSER_SIZE
  },
  "parsing": {
    "corpus_size": $TOTAL,
    "success": $SUCCESS,
    "failed": $FAILED,
    "success_rate": $SUCCESS_RATE,
    "total_errors": $TOTAL_ERRORS,
    "avg_errors": $AVG_ERRORS
  }
}
EOF

# Comparar com baseline
if [[ -f "$PROJECT_DIR/benchmark-baseline.json" ]]; then
    echo -e "\n[Comparação com baseline]"
    BASELINE_STATES=$(jq '.metrics.state_count' "$PROJECT_DIR/benchmark-baseline.json")
    BASELINE_SUCCESS=$(jq '.parsing.success_rate' "$PROJECT_DIR/benchmark-baseline.json")

    DIFF_STATES=$((STATE_COUNT - BASELINE_STATES))

    if [[ $DIFF_STATES -lt 0 ]]; then
        echo "  STATE_COUNT: $DIFF_STATES ✓ (melhorou)"
    elif [[ $DIFF_STATES -gt 0 ]]; then
        echo "  STATE_COUNT: +$DIFF_STATES ✗ (piorou)"
    else
        echo "  STATE_COUNT: 0 (igual)"
    fi

    echo "  SUCCESS_RATE: ${SUCCESS_RATE}% (baseline: ${BASELINE_SUCCESS}%)"
fi

echo -e "\n========================================="
echo "STATE=$STATE_COUNT | LARGE=$LARGE_STATE_COUNT | PARSE=${SUCCESS_RATE}%"
echo "========================================="

# Falhar se não tiver 100% de sucesso
if [[ "$SUCCESS_RATE" != "100.00" ]]; then
    echo -e "\n⚠️  ATENÇÃO: Taxa de sucesso não é 100%!"
    exit 1
fi

# Atualizar baseline automaticamente se melhorou ou é igual
if [[ -f "$PROJECT_DIR/benchmark-baseline.json" ]]; then
    if [[ $DIFF_STATES -le 0 ]]; then
        cp "$PROJECT_DIR/benchmark-report.json" "$PROJECT_DIR/benchmark-baseline.json"
        echo -e "\n✓ Baseline atualizado automaticamente"
    fi
else
    # Se não existe baseline, criar
    cp "$PROJECT_DIR/benchmark-report.json" "$PROJECT_DIR/benchmark-baseline.json"
    echo -e "\n✓ Baseline criado"
fi
