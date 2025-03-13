module.exports = {
  move_statement: ($) =>
    seq(
      kw("MOVE"),
      field("from", $._move_from),
      $._TO,
      field("to", repeat1($.variable)),
    ),

  _move_from: ($) =>
    seq(
      optional(
        choice(
          seq($._LENGTH, op($._OF)), //
          kw("FUNCTION"),
        ),
      ),
      $._expr_data,
    ),
};
