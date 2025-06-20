module.exports = {
  perform_statement: ($) =>
    seq(
      $._PERFORM,
      choice(
        $.perform_simple,
        $.perform_times,
        $.perform_until,
        $.perform_x_until,
        $.perform_varying,
      ),
    ),

  _PERFORM: (_) => kw("PERFORM"),
  _END_PERFORM: (_) => kw("END-PERFORM"),

  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  //  PERFORM 100000-FOO.
  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  perform_simple: ($) =>
    seq(
      field("label", $.section_name), //
      optional(seq($._THRU, $.section_name)),
    ),

  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  //  PERFORM 100 TIMES
  //      DISPLAY "FO"
  //  END-PERFORM.
  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  perform_times: ($) =>
    seq(
      seq(field("times", $.integer), kw("TIMES")),
      repeat($._statement),
      $._END_PERFORM,
    ),

  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  //  PERFORM UNTIL IND-FIM-610E = 1
  //       DISPLAY "FOO"
  //  END-PERFORM.
  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  perform_until: ($) =>
    seq(
      seq(kw("UNTIL"), $.expr), //
      repeat($._statement),
      $._END_PERFORM,
    ),

  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  //  PERFORM 200000-PROCESSA UNTIL CND-ARQUIVO-FIM.
  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  perform_x_until: ($) =>
    seq(
      field("label", $.section_name), //
      op($.varying),
      $._UNTIL,
      field("condition", $.expr),
    ),

  varying: ($) =>
    seq(
      $._VARYING,
      $.variable,
      $._FROM,
      field("from", $._expr_data),
      $._BY,
      field("by", $._expr_data),
    ),

  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  //  PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
  // ╾───────────────────────────────────────────────────────────────────────────────────╼

  perform_varying: ($) =>
    prec.right(
      seq(
        $.varying,
        $._UNTIL,
        field("until", $.expr),
        repeat($._statement),
        $._END_PERFORM,
      ),
    ),
};
