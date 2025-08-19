module.exports = {
  move_statement: ($) =>
    prec.left(
      seq(
        kw("MOVE"),
        op($._ALL),
        field("from", $._move_from),
        $._TO,
        repeat1(field("to", $.variable)),
      ),
    ),

  _move_from: ($) =>
    seq(
      op(choice($._CORRESPONDING, $._CORR)),
      op(choice(seq($._LENGTH, op($._OF)), $._FUNCTION)),
      $._expr_data,
    ),
};
