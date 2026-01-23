module.exports = {
  _PLAIN_WORD: (_) =>
    /([0-9][a-zA-Z0-9-]*[a-zA-Z][a-zA-Z0-9-]*)|([a-zA-Z][a-zA-Z0-9-]*)/,

  _WORD: ($) =>
    seq($._PLAIN_WORD, repeat(seq($._CONTINUATION_HYPHEN, $._PLAIN_WORD))),

  WORD: ($) => $._WORD,

  section_name: ($) => $._PLAIN_WORD,
  // variable: ($) =>
  //   seq(
  //     field("name", $._WORD), //
  //     field("index", op(paren(repeat1($._WORD)))),
  //   ),
  // subref: ($) => seq("(", $._exp_list, ")"),
  // refmod: ($) => seq("(", $.exp, ":", optional($.exp), ")"),

  // Flatten optionals: em vez de 2 optionals sequenciais, usar repeat
  // Isso reduz estados de 4 combinações para 1 loop
  variable: ($) =>
    prec.right(
      seq(
        field("name", sep1($._WORD, choice($._IN, $._OF))),
        repeat(choice(
          field("subscripts", $.subscript_reference),
          field("reference_modification", $.reference_modification),
        )),
      ),
    ),

  // `TABLE(I)`, `TABLE(I, J)`.
  // Usa _expr_calc (aritmético) em vez de expr (booleano) - subscripts são numéricos
  subscript_reference: ($) => paren(sep1($._expr_calc, ",")),

  // `TEXT-FIELD(5:10)`
  // Usa _expr_calc (aritmético) - posições são numéricas
  reference_modification: ($) =>
    paren(seq(field("start", $._expr_calc), ":", optional(field("length", $._expr_calc)))),

  //  ╾───────────────────────────────────────────────────────────────────────────────────╼
  file_name: ($) => $._WORD,

  // ╭──────────────────────────────────────────────────────────╮
  // │                        Data types                        │
  // ╰──────────────────────────────────────────────────────────╯
  value: ($) => $._value,
  _value: ($) => choice($.boolean, $.number, $.string, $.constant),
  boolean: ($) => choice($._TRUE, $._FALSE),
  number: ($) => choice($.integer, $.decimal),
  integer: (_) => /[+-]?[0-9]+/,
  decimal: (_) => /[+-]?[0-9]*[\.,][0-9]+/,

  string: ($) =>
    prec.right(
      seq(
        $._STRING_START,
        repeat(choice($._STRING_CONTENT, $._STRING_CONTINUATION)),
        $._STRING_END,
      ),
    ),

  constant: (_) =>
    choice(
      kw("LITERAL"),
      kw("SPACE"),
      kw("SPACES"),
      kw("ZERO"),
      kw("ZEROS"),
      kw("ZEROES"),
      kw("QUOTE"),
      kw("HIGH-VALUE"),
      kw("HIGH-VALUES"),
      kw("LOW-VALUE"),
      kw("LOW-VALUES"),
      kw("LOW-VALUES"),
    ),

  _END: (_) => kw("END"),
  _DIVISION: (_) => kw("DIVISION"),
  _SECTION: (_) => kw("SECTION"),
  _THRU: (_) => choice(kw("THRU"), kw("THROUGH")),
  _VALUES: (_) => kw("VALUES"),
  _UP: (_) => kw("UP"),
  _UNTIL: (_) => kw("UNTIL"),
  _TO: (_) => kw("TO"),
  _NOT: (_) => kw("NOT"),
  _AND: (_) => kw("AND"),
  _OR: (_) => kw("OR"),
  _OF: (_) => kw("OF"),
  _IS: (_) => kw("IS"),
  _SEQUENTIAL: (_) => kw("SEQUENTIAL"),
  _RELATIVE: (_) => kw("RELATIVE"),
  _RECORD: (_) => kw("RECORD"),
  _KEY: (_) => kw("KEY"),
  _CONTAINS: (_) => kw("CONTAINS"),
  _CHARACTERS: (_) => kw("CHARACTERS"),
};
