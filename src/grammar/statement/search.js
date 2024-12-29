module.exports = {
  search_statement: ($) =>
    prec.right(
      seq(
        kw("SEARCH"),
        optional(kw("ALL")),
        field("table_name", $.variable),
        optional(field("varying", seq(kw("VARING"), $.WORD))),
        repeat(
          prec.right(
            seq(
              repeat1(choice($.search_when, $.at_end)), //
              repeat($._statement),
            ),
          ),
        ),
        op(kw("END-SEARCH")),
      ),
    ),

  search_when: ($) => seq(kw("WHEN"), field("condition", $.expr)),
};
