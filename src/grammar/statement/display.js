module.exports = {
  display_statement: ($) =>
    prec.right(
      seq(
        kw("DISPLAY"),
        repeat1(choice($.boolean, $.number, $.variable, $.constant, $.string)),
        op("."),
      ),
    ),
};
