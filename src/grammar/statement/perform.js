module.exports = {
  perform_statement: ($) =>
    seq(
      choice(
        $.perform_simple, //
        $.perform_until,
        $.perform_x_until,
      ),
      op("."),
    ),

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
      seq($._END_PERFORM),
    ),

  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  //  PERFORM 200000-PROCESSA UNTIL CND-ARQUIVO-FIM.
  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  perform_x_until: ($) =>
    seq(
      seq(
        $._PERFORM,
        field("label", $.section_name),
        $._UNTIL,
        field("condition", $.expr),
      ),
    ),
};
