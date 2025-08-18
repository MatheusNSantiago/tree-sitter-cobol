module.exports = {
  perform_statement: ($) => choice(prec(1, $.perform_block), $.perform_sentence),

  //  ╭──────────────────────────────────────────────────────────╮
  //  │                          BLOCO                           │
  //  ╰──────────────────────────────────────────────────────────╯

  // Ex: PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10 ... END-PERFORM
  //     PERFORM 100 TIMES ... DISPLAY "FOO" ... END-PERFORM
  //     PERFORM UNTIL IND-FIM-610E = 1 ... DISPLAY "FOO" ... END-PERFORM
  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  perform_block: ($) =>
    prec.right(
      // Necessário para resolver aninhamento de blocos PERFORM
      seq(
        $._PERFORM,
        choice(
          seq(field("times", $.integer), $._TIMES),
          seq($._UNTIL, field("condition", $.expr)),
          seq($._perform_varying_clause, $._UNTIL, field("condition", $.expr)),
        ),
        field("body", $.statement_block),
        $._END_PERFORM,
      ),
    ),

  //  ╭──────────────────────────────────────────────────────────╮
  //  │                         SENTENÇA                         │
  //  ╰──────────────────────────────────────────────────────────╯
  // Ex: PERFORM 1000-ROTINA.
  //     PERFORM 1000-ROTINA THRU 2000-FIM-ROTINA.
  //     PERFORM 1000-ROTINA VARYING I FROM 1 BY 1 UNTIL I > 10.
  //     PERFORM 200000-PROCESSA UNTIL CND-ARQUIVO-FIM
  perform_sentence: ($) =>
    seq(
      seq($._PERFORM, field("label", $.section_name)),
      opseq($._THRU, field("thru", $.section_name)),
      opseq(
        op($._perform_varying_clause),
        seq($._UNTIL, field("condition", $.expr)),
      ),
    ),

  _perform_varying_clause: ($) =>
    seq(
      seq($._VARYING, $.variable),
      seq($._FROM, field("from", $._expr_data)),
      seq($._BY, field("by", $._expr_data)),
    ),

  // perform_statement: ($) =>
  //   choice(
  //     $._perform_simple,
  //     $._perform_times,
  //     $._perform_until,
  //     $._perform_x_until,
  //     $._perform_varying,
  //   ),
  //
  // // ╾───────────────────────────────────────────────────────────────────────────────────╼
  // //  PERFORM 100000-FOO.
  // // ╾───────────────────────────────────────────────────────────────────────────────────╼
  // perform_simple: ($) => $._perform_simple,
  // _perform_simple: ($) =>
  //   seq(
  //     $._PERFORM,
  //     field("label", $.section_name), //
  //     opseq($._THRU, field("thru", $.section_name)),
  //   ),
  //
  // // ╾───────────────────────────────────────────────────────────────────────────────────╼
  // //  PERFORM 100 TIMES
  // //      DISPLAY "FO"
  // //  END-PERFORM.
  // // ╾───────────────────────────────────────────────────────────────────────────────────╼
  // _perform_times: ($) =>
  //   seq(
  //     $._PERFORM,
  //     seq(field("times", $.integer), $._TIMES),
  //     field("body", $.statement_block),
  //     $._END_PERFORM,
  //   ),
  //
  // // ╾───────────────────────────────────────────────────────────────────────────────────╼
  // //  PERFORM UNTIL IND-FIM-610E = 1
  // //       DISPLAY "FOO"
  // //  END-PERFORM.
  // // ╾───────────────────────────────────────────────────────────────────────────────────╼
  // _perform_until: ($) =>
  //   seq(
  //     $._PERFORM,
  //     seq($._UNTIL, field("condition", $.expr)), //
  //     field("body", $.statement_block),
  //     $._END_PERFORM,
  //   ),
  //
  // // ╾───────────────────────────────────────────────────────────────────────────────────╼
  // //  PERFORM 200000-PROCESSA UNTIL CND-ARQUIVO-FIM.
  // // ╾───────────────────────────────────────────────────────────────────────────────────╼
  // _perform_x_until: ($) =>
  //   seq(
  //     $._PERFORM,
  //     field("label", $.section_name), //
  //     op($._perform_varying_clause),
  //     $._UNTIL,
  //     field("condition", $.expr),
  //   ),
  //
  // _perform_varying_clause: ($) =>
  //   seq(
  //     $._VARYING,
  //     $.variable,
  //     seq($._FROM, field("from", $._expr_data)),
  //     seq($._BY, field("by", $._expr_data)),
  //   ),
  //
  // // ╾───────────────────────────────────────────────────────────────────────────────────╼
  // //  PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
  // // ╾───────────────────────────────────────────────────────────────────────────────────╼
  //
  // _perform_varying: ($) =>
  //   prec.right(
  //     seq(
  //       seq(
  //         $._PERFORM,
  //         $._perform_varying_clause,
  //         $._UNTIL,
  //         field("condition", $.expr),
  //       ),
  //       field("body", $.statement_block),
  //       $._END_PERFORM,
  //     ),
  //   ),
};
