#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <tree_sitter/parser.h>

enum TokenType {
  COMMENT,
  WHITE_SPACES,
  PREFIX_COMMENT,
  PARAGRAPH_HEADER,
  SECTION_HEADER,
  INLINE_COMMENT,
  SUFFIX_COMMENT,
};

static inline void advance(TSLexer *lexer) { lexer->advance(lexer, false); }
static inline void skip(TSLexer *lexer) { lexer->advance(lexer, true); }

static inline bool match_next(TSLexer *lexer, char *string) {
  int size = strlen(string);

  for (int i = 0; i < size; i++) {
    char next_char = toupper(lexer->lookahead);
    if (next_char != string[i])
      return false;

    advance(lexer);
  }

  return true;
}

static inline bool end_token(TSLexer *lexer, enum TokenType token_type) {
  lexer->mark_end(lexer);
  lexer->result_symbol = token_type;
  return true;
}
static inline bool is_next_char_terminal(TSLexer *lexer) {
  return lexer->lookahead == '\n' || lexer->eof(lexer) || lexer->lookahead == 0;
}
static inline bool is_starting_inline_comment(TSLexer *lexer) {
  if (lexer->lookahead != '*')
    return false;

  advance(lexer);

  if (lexer->lookahead != '>')
    return false;

  return true;
}

static inline int get_column(TSLexer *lexer) {
  return lexer->get_column(lexer);
}
/* ╾───────────────────────────────────────────────────────────────────────────────────╼*/

/// O scanner não mantém estado, então não precisamos alocar memória
void *tree_sitter_cobol_external_scanner_create() { return NULL; }

/// O scanner não mantém estado, então não precisamos fazer nada aqui
void tree_sitter_cobol_external_scanner_destroy(void *payload) {}

bool tree_sitter_cobol_external_scanner_scan(void *payload, TSLexer *lexer,
                                             const bool *valid_symbols) {
  if (lexer->eof(lexer))
    return false;

  int *next_char = &(lexer->lookahead);

  /* ╭──────────────────────────────────────────────────────────╮
     │                        Blank Line                        │
     ╰──────────────────────────────────────────────────────────╯ */

  if (valid_symbols[WHITE_SPACES]) {
    if (isspace(*next_char)) {
      int lines = 0;
      int char_count = 0;

      // Pula todos os espaços em branco (\n, \t, \r, ' ')
      while (isspace(*next_char)) {
        if (*next_char == '\n')
          lines++;

        advance(lexer);
      }

      if (lines > 1)
        return end_token(lexer, WHITE_SPACES);
    }
  }

  /* ╭──────────────────────────────────────────────────────────╮
     │                      Prefix Comment                      │
     ╰──────────────────────────────────────────────────────────╯ */

  if (valid_symbols[PREFIX_COMMENT]) {
    while (get_column(lexer) <= 5) {
      if (!isspace(*next_char))
        advance(lexer);

      return end_token(lexer, PREFIX_COMMENT);
    }
  }

  /* ╭──────────────────────────────────────────────────────────╮
     │                      Normal Comment                      │
     ╰──────────────────────────────────────────────────────────╯ */
  int col = get_column(lexer);

  if (valid_symbols[COMMENT]) {
    if (col == 6) {

      bool is_starting_a_comment = *next_char == '*';
      if (is_starting_a_comment) {
        while (!is_next_char_terminal(lexer))
          advance(lexer);

        return end_token(lexer, COMMENT);
      }
    }
  }

  /* ╭──────────────────────────────────────────────────────────╮
     │                        PARAGRAPH                         │
     ╰──────────────────────────────────────────────────────────╯
  */

  if (valid_symbols[SECTION_HEADER]) {
    if (col == 7) {

      // Passa pelas labels. Ex: 100000-SECTION-NAME
      while (isalnum(*next_char) || *next_char == '-')
        advance(lexer);

      while (isspace(*next_char))
        advance(lexer);

      if (match_next(lexer, "SECTION.")) {
        return end_token(lexer, SECTION_HEADER);
      }
    }
  }


  if (valid_symbols[PARAGRAPH_HEADER]) {
    if (col == 7) {

      // Passa pelas labels. Ex: 100000-SECTION-NAME
      while (isalnum(*next_char) || *next_char == '-')
        advance(lexer);

      while (isspace(*next_char))
        advance(lexer);

      bool is_paragraph = *next_char == '.';
      if (is_paragraph) {
        advance(lexer);
        return end_token(lexer, PARAGRAPH_HEADER);
      }
    }
  }


  /* ╭──────────────────────────────────────────────────────────╮
     │                      Inline comment                      │
     ╰──────────────────────────────────────────────────────────╯
  */

  if (valid_symbols[INLINE_COMMENT]) {
    if (col >= 7 && col <= 71) {
      if (is_starting_inline_comment(lexer)) {
        while (!is_next_char_terminal(lexer))
          advance(lexer);

        return end_token(lexer, INLINE_COMMENT);
      }
    }
  }

  /* ╭──────────────────────────────────────────────────────────╮
     │                      Suffix Comment                      │
     ╰──────────────────────────────────────────────────────────╯ */

  if (valid_symbols[SUFFIX_COMMENT]) {
    while (get_column(lexer) >= 72) {
      if (!isspace(*next_char))
        advance(lexer);

      return end_token(lexer, SUFFIX_COMMENT);
    }
  }

  return false;
}

/// Guarda todo o estado estado desse scanner em um buffer
/// A função é chamada toda vez que esse scanner achar um token
unsigned tree_sitter_cobol_external_scanner_serialize(void *payload,
                                                      char *buffer) {
  return 0;
}

/// Restora o estado do scanner
void tree_sitter_cobol_external_scanner_deserialize(void *payload,
                                                    const char *buffer,
                                                    unsigned length) {}
