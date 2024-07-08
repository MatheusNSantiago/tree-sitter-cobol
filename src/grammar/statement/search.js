module.exports = {
  search_statement: ($) =>
    seq(
      kw("SEARCH"),
      optional(kw("ALL")),
      field("table_name", $.variable),
      optional(field("varying", seq(kw("VARING"), $.WORD))),
      repeat(
        seq(
          choice($.search_when, $.at_end),
          repeat($._statement), //
        ),
      ),
      kw("END-SEARCH"),
    ),

  search_when: ($) => seq(kw("WHEN"), field("condition", $.expr)),
};
