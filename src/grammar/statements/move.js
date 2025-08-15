module.exports = {
  move_statement: ($) =>
    prec.left(
      seq(
        kw("MOVE"),
        op($._ALL),
        field("from", $._move_from),
        $._TO,
        repeat1(
          seq(C($), field("to", $.variable)), //
        ),
      ),
    ),

  _move_from: ($) =>
    seq(
      op(kw("CORRESPONDING")),
      op(
        choice(
          seq($._LENGTH, op($._OF)), //
          kw("FUNCTION"),
        ),
      ),
      $._expr_data,
    ),
};
