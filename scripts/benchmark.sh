#!/bin/bash
# Benchmark script para tree-sitter-cobol

set -euo pipefail

COBOL_DIR="/home/matheus/dev/mainframe-doc/backend/data/sgt/cobol"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
CLEAN_SCRIPT="$SCRIPT_DIR/clean-cobol.sh"
SAMPLE_SIZE="${1:-50}"

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

# 3. Parsear arquivos COBOL (com limpeza)
echo -e "\n[3/4] Parseando $SAMPLE_SIZE arquivos..."

FILELIST=$(mktemp)
CLEANFILE=$(mktemp --suffix=.cbl)
ls "$COBOL_DIR"/*.cbl | shuf -n "$SAMPLE_SIZE" > "$FILELIST"

SUCCESS=0
FAILED=0
TOTAL_ERRORS=0
FAILED_LIST=""

while read -r f; do
    # Limpa o arquivo para tmp
    "$CLEAN_SCRIPT" "$f" > "$CLEANFILE"
    # Parseia
    OUTPUT=$(tree-sitter parse "$CLEANFILE" 2>&1 || true)
    ERROR_COUNT=$(echo "$OUTPUT" | grep -c "ERROR\|MISSING") || ERROR_COUNT=0

    if [[ $ERROR_COUNT -eq 0 ]]; then
        SUCCESS=$((SUCCESS + 1))
    else
        FAILED=$((FAILED + 1))
        TOTAL_ERRORS=$((TOTAL_ERRORS + ERROR_COUNT))
        FAILED_LIST="$FAILED_LIST $(basename "$f")"
    fi
done < "$FILELIST"

rm "$FILELIST" "$CLEANFILE"

TOTAL=$((SUCCESS + FAILED))
SUCCESS_RATE=$(awk "BEGIN {printf \"%.2f\", $SUCCESS * 100 / $TOTAL}")
AVG_ERRORS=$(awk "BEGIN {printf \"%.1f\", $TOTAL_ERRORS / $TOTAL}")

echo "  Success: $SUCCESS / $TOTAL (${SUCCESS_RATE}%)"
echo "  Failed:  $FAILED (avg errors: $AVG_ERRORS)"

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
    "sample_size": $TOTAL,
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
