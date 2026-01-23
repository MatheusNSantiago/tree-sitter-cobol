#!/bin/bash
# Limpa arquivo COBOL para parsing
# - Remove colunas 1-6 (sequência)
# - Remove colunas após 80
# - Converte -INC para COPY

sed -E '
  # Remove primeiras 6 colunas (substitui por espaços)
  s/^.{6}/      /

  # Converte -INC XXXXX para COPY XXXXX.
  s/^[[:space:]]*[-=]INC[[:space:]]+([a-zA-Z0-9]+)/       COPY \1./

  # Trunca em 80 colunas (ignora linhas com VRS no final - versionamento)
  /VRS[0-9]+[[:space:]]*$/! s/^(.{80}).*/\1/
' "$1"
