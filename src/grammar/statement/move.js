module.exports = {
  move_statement: ($) =>
    prec.right(
      seq(
        kw("MOVE"),
        field("from", $._anything),
        $._TO,
        field("to", repeat1($.variable)),
        o("."),
      ),
    ),
};
