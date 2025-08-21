module.exports = {
  evaluate_statement: ($) =>
    prec.right(
      seq(
        seq($._EVALUATE, field("value", $.expr)),
        repeat1($._evaluate_when_clause),
        op($._evaluate_other_clause),
        $._END_EVALUATE,
      ),
    ),

  // A cláusula WHEN dentro de um bloco EVALUATE pode conter qualquer statement.
  _evaluate_when_clause: ($) =>
    seq(
      $._WHEN,
      field("condition", $.expr),
      field("body", op($.statement_block)),
    ),

  _evaluate_other_clause: ($) =>
    seq(
      seq($._WHEN, $._OTHER), //
      field("body", op($.statement_block)),
    ),
};
