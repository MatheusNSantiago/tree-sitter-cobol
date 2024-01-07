module.exports = {
  WORD: ($) => $._WORD,
  _WORD: (_) => /[a-zA-Z0-9-]+/,
  // /([0-9][a-zA-Z0-9-]*[a-zA-Z][a-zA-Z0-9-]*)|([a-zA-Z][a-zA-Z0-9-]*)/,

  section_name: ($) => $._WORD,
  variable: ($) => $._WORD,
  file_name: ($) => $._WORD,

  // ╭──────────────────────────────────────────────────────────╮
  // │                        Data types                        │
  // ╰──────────────────────────────────────────────────────────╯
  _value: ($) => choice($.boolean, $.number, $.string, $.constant),
  boolean: ($) => choice($._TRUE, $._FALSE),
  number: ($) => choice($.integer, $.decimal),
  integer: (_) => /[+-]?[0-9,]+/,
  decimal: (_) => /[+-]?[0-9]*[\.,][0-9]+/,
  string: (_) => choice(/('[^'\n]*')+/, /("[^"\n]*")+/),

  constant: (_) =>
    choice(
      kw("LITERAL"),
      kw("SPACE"),
      kw("SPACES"),
      kw("ZERO"),
      kw("ZEROS"),
      // kw("QUOTE"),
      kw("HIGH-VALUE"),
      kw("LOW-VALUE"),
    ),

  // ╾───────────────────────────────────────────────────────────────────────────────────╼

  _ALPHABETIC: (_) => kw("ALPHABETIC"),
  _AT: (_) => kw("AT"),
  _END: (_) => kw("END"),
  _PROCEDURE: (_) => kw("PROCEDURE"),
  _DIVISION: (_) => kw("DIVISION"),
  _SECTION: (_) => kw("SECTION"),
  _THRU: (_) => choice(kw("THRU"), kw("THROUGH")),
  _UNTIL: (_) => kw("UNTIL"),
  _TO: (_) => kw("TO"),
  _NOT: (_) => kw("NOT"),
  _AND: (_) => kw("AND"),
  _OR: (_) => kw("OR"),
  _IS: (_) => kw("IS"),
};
