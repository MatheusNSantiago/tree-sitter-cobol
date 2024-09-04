paren = (thing) => seq("(", thing, ")");
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}

module.exports = {
  _exec_statement: ($) => choice($.exec_cics, $.exec_sql),
  _EXEC: (_) => kw("EXEC"),

  // ╭──────────────────────────────────────────────────────────╮
  // │                         EXEC SQL                         │
  // ╰──────────────────────────────────────────────────────────╯

  exec_sql: ($) =>
    prec.right(seq($._EXEC, kw("SQL"), repeat($._exec_sql_body), kw("END-EXEC"))),

  _exec_sql_body: ($) => prec.right(15, seq($._exec_sql_statements, C($))),
  _exec_sql_statements: ($) =>
    choice(
      field(
        "declare",
        seq(kw("DECLARE"), $.cursor_name, kw("CURSOR"), kw("FOR")),
      ),
      field("select", seq(kw("SELECT"), sep1($.tab_field, ","))),
      field("into", seq(kw("INTO"), sep1(seq(":", $.variable), ","))),
      field("from", seq(kw("FROM"), $.tab_name)),
      field("where", seq(kw("WHERE"), $.expr)),
      field(
        "order",
        seq(
          kw("ORDER"),
          kw("BY"),
          sep1(seq($.tab_field, op(choice(kw("ASC"), kw("DEC")))), ","),
        ),
      ),
    ),

  cursor_name: (_) => /[a-zA-Z0-9_-]+/,
  tab_field: (_) => /[a-zA-Z0-9_]+/,
  tab_name: ($) => seq($._tab_name, ".", $._tab_name),
  _tab_name: (_) => /[a-zA-Z0-9_]+/,

  // ╭──────────────────────────────────────────────────────────╮
  // │                        EXEC CICS                         │
  // ╰──────────────────────────────────────────────────────────╯
  exec_cics: ($) =>
    seq(
      seq(
        $._EXEC,
        kw("CICS"),
        optional(choice(kw("PUT"), kw("ASSIGN"), kw("GET"))),
      ),
      repeat($._exec_cics_body),
      kw("END-EXEC"),
    ),

  _exec_cics_body: ($) => prec.right(15, seq($._exec_cics_statements, C($))),
  _exec_cics_statements: ($) =>
    choice(
      seq(kw("CONTAINER"), paren($.string)),
      seq(kw("INTO"), paren($.variable)),
      seq(kw("FROM"), paren($.variable)),
      seq(kw("CHANNEL"), paren($.variable)),
      seq(kw("RESP"), paren($.variable)),
      seq(
        kw("FLENGTH"),
        paren(choice(seq(kw("LENGTH"), kw("OF"), $.variable), $.variable)),
      ),
      kw("RETURN"),
      kw("NOHANDLE"),
      kw("NODATA"),
    ),
};
