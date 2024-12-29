#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include "tree_sitter/parser.h"

enum TokenType {
  // COMMENT,
  BLANK_LINE,
  WHITE_SPACES,
  PREFIX,
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
  return ((lexer->lookahead == '\n') || (lexer->lookahead == '\r') ||
          (lexer->eof(lexer)) || (lexer->lookahead == 0));
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

  if (valid_symbols[BLANK_LINE]) {
    if (get_column(lexer) == 0) {
      if (*next_char == '\n') {
        advance(lexer);
        return end_token(lexer, BLANK_LINE);
      }
    }
  }

  /* ╭──────────────────────────────────────────────────────────╮
     │                       WHITE SPACES                       │
     ╰──────────────────────────────────────────────────────────╯
  */

  if (valid_symbols[WHITE_SPACES]) {
    if (isspace(*next_char)) {
      while (isspace(*next_char)) {
        if (*next_char == '\n') {
          advance(lexer);
          return end_token(lexer, WHITE_SPACES);
        }
        advance(lexer);
      }
    }
  }
  /* ╭──────────────────────────────────────────────────────────╮
     │                      Prefix Comment                      │
     ╰──────────────────────────────────────────────────────────╯ */

  if (valid_symbols[PREFIX]) {
    if (get_column(lexer) == 0) {
      while (get_column(lexer) <= 5 && !is_next_char_terminal(lexer))
        advance(lexer);

      return end_token(lexer, PREFIX);
    }
  }

  /* ╭──────────────────────────────────────────────────────────╮
     │                      Normal Comment                      │
     ╰──────────────────────────────────────────────────────────╯ */

  // if (valid_symbols[COMMENT]) {
  //   if (get_column(lexer) == 6) {
  //
  //     bool is_starting_a_comment = *next_char == '*';
  //     if (is_starting_a_comment) {
  //       while (!is_next_char_terminal(lexer))
  //         advance(lexer);
  //
  //       return end_token(lexer, COMMENT);
  //     }
  //   }
  // }

  /* ╭──────────────────────────────────────────────────────────╮
     │                  Paragraph / Sections                    │
     ╰──────────────────────────────────────────────────────────╯
  */

  if (valid_symbols[SECTION_HEADER] && valid_symbols[PARAGRAPH_HEADER]) {
    if (get_column(lexer) == 7) {
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

      bool is_section = match_next(lexer, "SECTION.");
      if (is_section)
        return end_token(lexer, SECTION_HEADER);
    }
  }

  /* ╭──────────────────────────────────────────────────────────╮
     │                      Inline comment                      │
     ╰──────────────────────────────────────────────────────────╯
  */

  if (valid_symbols[INLINE_COMMENT]) {
    int col = get_column(lexer);

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
