module.exports = {
  WORD: ($) => $._WORD,
  _WORD: (_) =>
    /([0-9][a-zA-Z0-9-]*[a-zA-Z][a-zA-Z0-9-]*)|([a-zA-Z][a-zA-Z0-9-]*)/,

  section_name: ($) => $._WORD,
  variable: ($) => $._WORD,

  // ╭──────────────────────────────────────────────────────────╮
  // │                        Data types                        │
  // ╰──────────────────────────────────────────────────────────╯

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
};
