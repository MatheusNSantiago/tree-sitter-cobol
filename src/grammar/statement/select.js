module.exports = {
  select_statement: ($) =>
    seq(
      kw("SELECT"),
      $.file_name,
      seq($._select_clause, "."), //
    ),
  _select_clause: ($) => choice($.assign_clause),

  assign_clause: ($) => seq(kw("ASSIGN"), $._TO, $._WORD),
};
