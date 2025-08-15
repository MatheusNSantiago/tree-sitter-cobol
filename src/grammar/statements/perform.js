module.exports = {
  perform_statement: ($) =>
    choice(
      $._perform_simple,
      $._perform_times,
      $._perform_until,
      $._perform_x_until,
      $._perform_varying,
    ),

  _PERFORM: (_) => kw("PERFORM"),
  _END_PERFORM: (_) => kw("END-PERFORM"),

  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  //  PERFORM 100000-FOO.
  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  perform_simple: ($) => $._perform_simple,
  _perform_simple: ($) =>
    seq(
      $._PERFORM,
      field("label", $.section_name), //
      opseq($._THRU, field("thru", $.section_name)),
    ),

  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  //  PERFORM 100 TIMES
  //      DISPLAY "FO"
  //  END-PERFORM.
  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  _perform_times: ($) =>
    seq(
      $._PERFORM,
      seq(field("times", $.integer), kw("TIMES")),
      $.perform_body,
      $._END_PERFORM,
    ),

  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  //  PERFORM UNTIL IND-FIM-610E = 1
  //       DISPLAY "FOO"
  //  END-PERFORM.
  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  _perform_until: ($) =>
    seq(
      $._PERFORM,
      seq($._UNTIL, field("condition", $.expr)), //
      $.perform_body,
      $._END_PERFORM,
    ),

  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  //  PERFORM 200000-PROCESSA UNTIL CND-ARQUIVO-FIM.
  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  _perform_x_until: ($) =>
    seq(
      $._PERFORM,
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

  _perform_varying: ($) =>
    prec.right(
      seq(
        seq($._PERFORM, $.varying, $._UNTIL, field("condition", $.expr)),
        $.perform_body,
        $._END_PERFORM,
      ),
    ),

  perform_body: ($) => repeat1($._statement),
};
