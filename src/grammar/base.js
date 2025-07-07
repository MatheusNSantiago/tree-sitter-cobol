module.exports = {
  WORD: ($) => $._WORD,
  _WORD: (_) =>
    /([0-9][a-zA-Z0-9-]*[a-zA-Z][a-zA-Z0-9-]*)|([a-zA-Z][a-zA-Z0-9-]*)/,

  section_name: ($) => $._WORD,
  variable: ($) => seq(field("name", $._WORD), op(paren($._WORD))),
  // subref: ($) => seq("(", $._exp_list, ")"),
  // refmod: ($) => seq("(", $.exp, ":", optional($.exp), ")"),

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

  string: (_) =>
    choice(
      /('[^'\n]*')+/, //
      /("[^"\n]*")+/,
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

  // ╾───────────────────────────────────────────────────────────────────────────────────╼

  _AT: (_) => kw("AT"),
  _END: (_) => kw("END"),
  _DIVISION: (_) => kw("DIVISION"),
  _SECTION: (_) => kw("SECTION"),
  _THRU: (_) => choice(kw("THRU"), kw("THROUGH")),
  _VALUES: (_) => kw("VALUES"),
  _VALUE: (_) => kw("VALUE"),
  _UP: (_) => kw("UP"),
  _UNTIL: (_) => kw("UNTIL"),
  _TO: (_) => kw("TO"),
  _NOT: (_) => kw("NOT"),
  _AND: (_) => kw("AND"),
  _OR: (_) => kw("OR"),
  _OF: (_) => kw("OF"),
  _IS: (_) => kw("IS"),
  _INTO: (_) => kw("INTO"),
  _SIZE: (_) => kw("SIZE"),
  _BY: (_) => kw("BY"),
  _SEQUENTIAL: (_) => kw("SEQUENTIAL"),
  _RELATIVE: (_) => kw("RELATIVE"),
  _RECORD: (_) => kw("RECORD"),
  _KEY: (_) => kw("KEY"),
  _CONTAINS: (_) => kw("CONTAINS"),
  _CHARACTERS: (_) => kw("CHARACTERS"),
};
