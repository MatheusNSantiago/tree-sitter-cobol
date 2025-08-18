#include "tree_sitter/parser.h"
#include <ctype.h>
#include <stdbool.h>
#include <string.h>

enum TokenType {
  STRING_CONTINUATION,
  CONTINUATION_HYPHEN,
  STRING_START,
  STRING_CONTENT,
  STRING_END,
  //////////////////
  BLANK_LINE,
  PREFIX,
  LINE_COMMENT,
  PARAGRAPH_HEADER,
  SECTION_HEADER,
  INLINE_COMMENT,
  SUFFIX_COMMENT,
};

typedef struct {
  char open_quote_char;
} Scanner;

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

static inline bool is_inline_space(char c) {
  return isspace(c) && c != '\n' && c != '\r';
}
/* ╾───────────────────────────────────────────────────────────────────────────────────╼*/

void *tree_sitter_cobol_external_scanner_create() {
  Scanner *scanner = (Scanner *)calloc(1, sizeof(Scanner));
  return scanner;
}

void tree_sitter_cobol_external_scanner_destroy(void *payload) {
  Scanner *scanner = (Scanner *)payload;
  free(scanner);
}

bool tree_sitter_cobol_external_scanner_scan(void *payload, TSLexer *lexer,
                                             const bool *valid_symbols) {
  Scanner *scanner = (Scanner *)payload;

  int *next_char = &(lexer->lookahead);

  // Primeiro, lide com o estado "dentro da string", que é o mais complexo.
  bool is_inside_string = scanner->open_quote_char != 0;
  if (is_inside_string) {
    bool has_content = false;

    // Consume o conteúdo da string na linha atual.
    while ((*next_char != scanner->open_quote_char) && // Não fechou aspa
           (*next_char != '\n') &&                     // não é quebra de linha
           (*next_char != '\r') &&                     // não é retorno de carro
           (!lexer->eof(lexer))                        // não é EOF
    ) {
      has_content = true;
      advance(lexer);
    }

    if (has_content) {
      return end_token(lexer, STRING_CONTENT);
    }

    bool has_found_end_quote = *next_char == scanner->open_quote_char;
    if (has_found_end_quote) {
      advance(lexer);

      scanner->open_quote_char = 0;
      return end_token(lexer, STRING_END);
    }

    // Se chegou aqui, só pode ser uma quebra de linha ou EOF.
    bool is_line_break_or_eof = *next_char == '\n' || *next_char == '\r';
    if (is_line_break_or_eof) {
      // Se a gramática espera uma continuação, vamos verificar.
      if (valid_symbols[STRING_CONTINUATION]) {
        skip(lexer); // Pula o \n

        // Pula espaços em branco iniciais na nova linha
        while (is_inline_space(*next_char)) {
          skip(lexer);
        }

        // Agora verifica o indicador de continuação
        bool is_continuation_indicator =
            get_column(lexer) == 6 && *next_char == '-';
        if (is_continuation_indicator) {
          skip(lexer); // Pula o hífen

          while (is_inline_space(*next_char)) {
            skip(lexer); // Pula espaços depois do hífen
          }

          bool has_found_end_quote = *next_char == scanner->open_quote_char;
          if (has_found_end_quote) {
            skip(lexer); // Pula a aspa de abertura da nova linha

            return end_token(lexer, STRING_CONTINUATION);
          }
        }
      }
    }

    // Se nada disso funcionou, deu ruim.
    return false;
  }

  // Se NÃO estamos em uma string, executamos a lógica normal.
  // Pula os espaços em branco que o `extras` do Tree-sitter pode ter perdido.
  while (isspace(*next_char)) {
    skip(lexer);
  }

  if (lexer->eof(lexer)) {
    return false;
  }

  // ╭──────────────────────────────────────────────────────────╮
  // │                        Blank Line                        │
  // ╰──────────────────────────────────────────────────────────╯

  if (valid_symbols[BLANK_LINE]) {
    if (get_column(lexer) == 0) {
      if (*next_char == '\n' || *next_char == '\r') {
        advance(lexer);

        return end_token(lexer, BLANK_LINE);
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

  //  ╭──────────────────────────────────────────────────────────╮
  //  │     Indicador da coluna 7 (comentários/continuação)      │
  //  ╰──────────────────────────────────────────────────────────╯

  if (get_column(lexer) == 6) {
    // Checar por hífen de continuação
    if (valid_symbols[CONTINUATION_HYPHEN]) {
      if (*next_char == '-') {
        advance(lexer);

        return end_token(lexer, CONTINUATION_HYPHEN);
      }
    }

    // Checar por comentário de linha
    if (valid_symbols[LINE_COMMENT]) {
      if (!isspace(*next_char)) {
        while (!is_next_char_terminal(lexer) && get_column(lexer) <= 71)
          advance(lexer);

        return end_token(lexer, LINE_COMMENT);
      }
    }
  }

  //  ╭──────────────────────────────────────────────────────────╮
  //  │                          Area A                          │
  //  ╰──────────────────────────────────────────────────────────╯

  if (valid_symbols[SECTION_HEADER] && valid_symbols[PARAGRAPH_HEADER]) {
    if (get_column(lexer) >= 7 && get_column(lexer) <= 10) {
      while (is_inline_space(*next_char))
        advance(lexer);

      // Passa pelas labels. Ex: 100000-SECTION-NAME
      while (isalnum(*next_char) || *next_char == '-')
        advance(lexer);

      while (is_inline_space(*next_char))
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

  //  ╭──────────────────────────────────────────────────────────╮
  //  │                          Area B                          │
  //  ╰──────────────────────────────────────────────────────────╯

  int current_column = get_column(lexer);
  if (current_column > 10) {
    // Checa por início de string literal
    if (valid_symbols[STRING_START]) {
      if (*next_char == '\'' || *next_char == '"') {
        scanner->open_quote_char = *next_char;
        advance(lexer);

        return end_token(lexer, STRING_START);
      }
    }

    // Checa por comentário inline
    if (valid_symbols[INLINE_COMMENT]) {
      if (current_column <= 71) {
        if (is_starting_inline_comment(lexer)) {
          while (!is_next_char_terminal(lexer))
            advance(lexer);

          return end_token(lexer, INLINE_COMMENT);
        }
      }
    }
  }

  // ╭──────────────────────────────────────────────────────────╮
  // │                      Suffix Comment                      │
  // ╰──────────────────────────────────────────────────────────╯

  if (valid_symbols[SUFFIX_COMMENT]) {
    if (get_column(lexer) >= 72) {
      while (!is_next_char_terminal(lexer))
        advance(lexer);

      return end_token(lexer, SUFFIX_COMMENT);
    }
  }

  return false;
}

/// A função é chamada toda vez que esse scanner achar um token
unsigned tree_sitter_cobol_external_scanner_serialize(void *payload,
                                                      char *buffer) {
  Scanner *scanner = (Scanner *)payload;
  buffer[0] = scanner->open_quote_char;
  return 1;
}

/// Restora o estado do scanner
void tree_sitter_cobol_external_scanner_deserialize(void *payload,
                                                    const char *buffer,
                                                    unsigned length) {
  Scanner *scanner = (Scanner *)payload;
  if (length > 0) {
    scanner->open_quote_char = buffer[0];
  } else {
    scanner->open_quote_char = 0;
  }
}
