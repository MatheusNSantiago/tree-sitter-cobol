op = (thing) => optional(thing);
C = ($) => repeat(choice($._comment, $._BLANK_LINE));
kw = (keyword) => keyword.toUpperCase();
// kw = (keyword) => {
// regex = new RegExp(
//   keyword
//     .split("")
//     .map((c) => `[${c.toLowerCase()}${c.toUpperCase()}]`)
//     .join(""),
// );
// return regex;
// };
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
  ],
  rules: {
    source_file: ($) =>
      seq(
        repeat($.directive),
        $.identification_division,
        optional($.environment_division),
        optional($.data_division),
        optional($.procedure_division),
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
