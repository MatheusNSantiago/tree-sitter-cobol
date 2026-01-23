module.exports = {
  move_statement: ($) =>
    prec.left(
      seq(
        $._MOVE,
        field("from", $._move_from),
        $._TO,
        repeat1(field("to", $.variable)),
      ),
    ),

  // MOVE patterns são mutuamente exclusivos:
  // MOVE value TO vars
  // MOVE ALL value TO vars
  // MOVE CORR source TO dest
  // MOVE LENGTH OF source TO dest
  _move_from: ($) =>
    choice(
      seq(choice($._CORRESPONDING, $._CORR), $._expr_data),
      seq($._LENGTH, optional($._OF), $._expr_data),
      seq($._ALL, $._expr_data),
      $._expr_data,
    ),
};
