module.exports = {
  perform_statement: ($) => choice($.perform_simple, $.perform_until),

  _PERFORM: (_) => kw("PERFORM"),
  _END_PERFORM: (_) => kw("END-PERFORM"),

  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  //  PERFORM 100000-FOO.
  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  perform_simple: ($) =>
    seq(
      $._PERFORM,
      field("label", $.section_name),
      optional(seq($._THRU, $.section_name)),
      optional("."),
    ),

  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  //  PERFORM UNTIL IND-FIM-610E = 1
  //       DISPLAY "FOO"
  //  END-PERFORM.
  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  perform_until: ($) =>
    seq(
      seq($._PERFORM, kw("UNTIL"), $.expr),
      repeat($._statement),
      seq($._END_PERFORM, optional(".")),
    ),
};
