module.exports = {
  select_statement: ($) =>
    seq(
      kw("SELECT"),
      field("file", $.file_name),
      seq($._select_clause, "."), //
    ),
  _select_clause: ($) => choice($._assign_clause),

  _assign_clause: ($) => seq(kw("ASSIGN"), $._TO, field("dd_name", $.WORD)),
};
