module.exports = {
  search_statement: ($) =>
    prec.right(
      seq(
        kw("SEARCH"),
        optional(kw("ALL")),
        field("table_name", $.variable),
        optional(field("varying", seq(kw("VARING"), $.WORD))),
        repeat1(
          choice($.search_when, $.at_end), //
        ),
        op(kw("END-SEARCH")),
      ),
    ),

  search_when: ($) =>
    prec.right(
      seq(
        kw("WHEN"),
        field("condition", $.expr),
        repeat($._statement),
      ),
    ),
};
