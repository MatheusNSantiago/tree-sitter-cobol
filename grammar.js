op = (thing) => optional(thing);
C = ($) => repeat(choice($._PREFIX, $._comment, $._BLANK_LINE));
kw = (keyword) => choice(keyword.toUpperCase(), keyword.toLowerCase());
aspas = (thing) =>
  choice(
    seq(op("'"), thing, op("'")),
    seq(op('"'), thing, op('"')), //
  );
paren = (thing) => seq("(", thing, ")");
parenOrNot = (thing) => choice(seq("(", thing, ")"), thing);
opseq = (...things) => optional(seq(...things));
sep1 = (rule, separator) => seq(rule, repeat(seq(separator, rule)));

module.exports = grammar({
  name: "cobol",
  externals: ($) => [
    $._STRING_CONTINUATION,
    $._CONTINUATION_HYPHEN,
    $._STRING_START,
    $._STRING_CONTENT,
    $._STRING_END,
    $._BLANK_LINE,
    $._PREFIX,
    $.copy_include,
    $.comment,
    $.paragraph_header,
    $.section_header,
    $._INLINE_COMMENT,
    $._SUFFIX_COMMENT,
    $._SQL_EXEC_START,
    $._SQL_EXEC_END,
    $.sql_line_comment,
    $.sql_block_comment,
  ],
  extras: ($) => [
    /\s/, // Espaços em branco padrão
    $._PREFIX, // Area de sequencia fixed-format: 000010, 000020, ...
    $.comment, // Comentários de linha inteira '*' na col 7
    $._INLINE_COMMENT, // Comentários '*>'
    $._SUFFIX_COMMENT, // Comentários na área de sufixo
    $._BLANK_LINE,
    $._CONTINUATION_HYPHEN,
    $.sql_line_comment,
    $.sql_block_comment,
  ],
  conflicts: ($) => [
    [$.sql_between_expression, $.sql_binary_expression, $.sql_like_expression],
    [$._fragment, $._imperative_sentences],
    [$._fragment, $.copy_statement],
    [$._fragment],
    [$._sentence],
    [$._data_division_statements],
  ],
  rules: {
    // Entry point: programa completo OU fragmento (copybook, hover snippet, etc.).
    source_file: ($) => seq(C($), choice(prec(10, $._program), $._fragment)),

    _program: ($) =>
      seq(
        repeat($.directive),
        $.identification_division,
        optional($.environment_division),
        optional($.data_division),
        optional($.procedure_division),
      ),

    // Fragmento sem cabeçalhos: section/paragraph/sentence/data description soltos.
    _fragment: ($) =>
      seq(
        C($),
        repeat1(
          prec.right(seq(
            choice(
              $._PREFIX,
              seq($._PREFIX, $._comment),
              prec(4, choice(
                seq(alias($._copy_include_statement, $.copy_statement), "."),
                alias($._copy_include_statement, $.copy_statement),
              )),
              $.section,
              $.paragraph,
              seq($.exec_sql, op(".")),
              $._sentence,
              seq(op($._PREFIX), $.data_description, op(".")),
              prec(3, choice(seq($.copy_statement, "."), $.copy_statement)),
            ),
            C($),
          )),
        ),
      ),

    _comment: ($) => $.comment,

    directive: (_) => choice(seq(kw("CBL"), "ARITH(EXTEND)")),

    ...require("./src/grammar/division/identification"),
    ...require("./src/grammar/division/environment"),
    ...require("./src/grammar/division/data"),
    ...require("./src/grammar/division/procedure"),
    ...require("./src/grammar/keywords"),
    // ╾───────────────────────────────────────────────────────────────────────────────────╼
    ...require("./src/grammar/base"),
    ...require("./src/grammar/expression"),
    ...require("./src/grammar/statements/"),
  },
});
