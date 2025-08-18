module.exports = {
  // evaluate_statement: ($) =>
  //   seq(
  //     seq($._EVALUATE, $.expr), // EVALUEATE <expr>
  //     C($),
  //     repeat1(
  //       field(
  //         "case",
  //         seq(
  //           seq($._WHEN, $.expr), // WHEN <expr>
  //           field("body", op($.statement_block)),
  //         ),
  //       ),
  //     ),
  //     field(
  //       "other",
  //       opseq(
  //         seq($._WHEN, $._OTHER), //
  //         C($),
  //         field("body", op($.statement_block)),
  //       ),
  //     ),
  //     $._END_EVALUATE,
  //   ),

  evaluate_statement: ($) =>
    prec.right(
      seq(
        seq($._EVALUATE, $.expr),
        repeat1($._evaluate_when_clause),
        op($._evaluate_other_clause),
        $._END_EVALUATE,
      ),
    ),

  // A cláusula WHEN dentro de um bloco EVALUATE pode conter qualquer statement.
  _evaluate_when_clause: ($) =>
    seq($._WHEN, $.expr, field("body", op($.statement_block))),

  _evaluate_other_clause: ($) =>
    seq(
      seq($._WHEN, $._OTHER), //
      field("body", op($.statement_block)),
    ),
};
