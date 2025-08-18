module.exports = {
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
          seq(field("times", choice($.integer, $.variable)), $._TIMES),
          seq($._UNTIL, field("condition", $.expr)),
          seq($._perform_varying_until_clause),
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
  //     PERFORM 200000-PROCESSA WS-CNT TIMES
  //     PERFORM 200000-PROCESSA UNTIL CND-ARQUIVO-FIM
  perform_sentence: ($) =>
    seq(
      $._PERFORM,
      seq(field("label", $.section_name)),
      optional(
        choice(
          seq($._THRU, field("thru", $.section_name)),
          $._perform_varying_until_clause,
          seq(field("times", choice($.integer, $.variable)), $._TIMES),
          seq($._UNTIL, field("condition", $.expr)),
        ),
      ),
    ),

  _perform_varying_until_clause: ($) =>
    seq(
      seq($._VARYING, $.variable),
      seq($._FROM, field("from", $._expr_data)),
      seq($._BY, field("by", $._expr_data)),
      seq($._UNTIL, field("condition", $.expr)),
    ),
};
