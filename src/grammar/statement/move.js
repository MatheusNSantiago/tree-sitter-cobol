module.exports = {
  move_statement: ($) =>
    prec.right(
      seq(
        kw("MOVE"),
        field("from", choice($.variable, $._value)),
        $._TO,
        field("to", repeat1($.variable)),
        optional("."),
      ),
    ),
};
