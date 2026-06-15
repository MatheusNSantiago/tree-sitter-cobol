module.exports = {
  evaluate_statement: ($) =>
    prec.right(
      seq(
        seq($._EVALUATE, $.evaluate_subjects),
        repeat1($._evaluate_when_clause),
        op($._evaluate_other_clause),
        $._END_EVALUATE,
      ),
    ),

  evaluate_subjects: ($) => sep1(field("value", $.expr), $._ALSO),

  // A cláusula WHEN dentro de um bloco EVALUATE pode conter qualquer statement.
  _evaluate_when_clause: ($) =>
    seq(
      $._WHEN,
      sep1(field("condition", $.expr), $._ALSO),
      field("body", op($.statement_block)),
    ),

  _evaluate_other_clause: ($) =>
    seq(
      seq($._WHEN, $._OTHER), //
      field("body", op($.statement_block)),
    ),
};
