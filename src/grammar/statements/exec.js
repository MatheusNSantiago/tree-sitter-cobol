module.exports = {
  _exec_statement: ($) => choice($.exec_cics, $.exec_sql),
  _EXEC: (_) => kw("EXEC"),

  // ╭──────────────────────────────────────────────────────────╮
  // │                         EXEC SQL                         │
  // ╰──────────────────────────────────────────────────────────╯

  exec_sql: ($) =>
    prec.right(
      seq(
        seq($._EXEC, kw("SQL")),
        repeat($._exec_sql_body), //
        kw("END-EXEC"),
      ),
    ),

  _exec_sql_body: ($) =>
    seq(
      choice(
        $._exec_sql_declare,
        $._exec_sql_statements, //
      ),
      C($),
    ),

  _exec_sql_declare: ($) =>
    seq(
      kw("DECLARE"),
      prec.right(
        15,
        choice(
          // field("declare_table", $._sql_declare_table),
          field("declare_cursor", $._sql_declare_cursor),
        ),
      ),
    ),

  _sql_declare_table: ($) =>
    seq(
      field("table", $.tab_name),
      kw("TABLE"),
      parenOrNot(
        sep1(
          seq(
            field("column_name", $.sql_identifier),
            field("data_type", $.sql_data_type),
            optional(field("constraint", $.sql_constraint)),
          ),
          ",",
        ),
      ),
    ),

  _sql_declare_cursor: ($) =>
    seq(
      $.cursor_name,
      seq(kw("CURSOR"), opseq(kw("WITH"), kw("HOLD")), $._FOR),
    ),

  sql_data_type: ($) =>
    choice(
      kw("SMALLINT"),
      kw("INTEGER"),
      seq(kw("DECIMAL"), paren(seq($.number, opseq(",", $.number)))),
      seq(kw("CHAR"), paren($.number)),
      kw("DATE"),
      kw("TIME"),
    ),

  sql_constraint: ($) => seq(op($._NOT), kw("NULL")),

  _exec_sql_statements: ($) =>
    choice(
      field("include", seq(kw("INCLUDE"), $.variable)),
      choice(
        seq(field("operation", $.SELECT), op($._sql_tab_fields)), // SELECT
        seq(field("operation", $.UPDATE), field("table", $.tab_name)), // UPDATE
        seq(field("operation", $.DELETE), $._FROM, field("table", $.tab_name)), // UPDATE
        seq(
          field("operation", $.INSERT),
          $._INTO,
          field("table", $.tab_name),
          $._sql_tab_fields,
        ), // INSERT
      ),
      field("fetch", seq(kw("FETCH"), $.cursor_name)),
      field("values", seq($._VALUES, $._sql_values)),
      field("into", seq($._INTO, $._sql_values)),
      field("set", seq(kw("SET"), sep1($.expr, ","))),
      field(
        "from",
        seq(
          $._FROM,
          sep1(
            seq(field("table", $.tab_name), optional($._WORD)), // alias
            ",",
          ),
        ),
      ),
      field("where", seq(kw("WHERE"), $.expr)),
      field(
        "for_update",
        seq($._FOR, kw("UPDATE"), opseq($._OF, $._sql_tab_fields)),
      ),
      // field(
      //   "limit",
      //   seq(kw("FETCH"), kw("FIRST"), $.number, kw("ROWS"), op(kw("ONLY"))),
      // ),
      field(
        "order",
        seq(
          kw("ORDER"),
          kw("BY"),
          sep1(seq($.tab_field, op(choice(kw("ASC"), kw("DEC")))), ","),
        ),
      ),
    ),

  _sql_tab_fields: ($) => parenOrNot(sep1($.tab_field, ",")),
  _sql_values: ($) => parenOrNot(sep1(seq(":", $.variable), ",")),

  cursor_name: (_) => /[a-zA-Z0-9_-]+/,
  tab_field: (_) =>
    prec.left(
      seq(
        opseq(/[a-zA-Z0-9]+/, "."), // Alias
        /[a-zA-Z0-9_\*\(\)]+/, // Nome do campo
        // op(paren(/[a-zA-Z0-9_\*]+/)),
      ),
    ),
  tab_name: ($) => seq($._sql_identifier, ".", $._sql_identifier),
  _sql_identifier: ($) => $.sql_identifier,
  sql_identifier: (_) => /[a-zA-Z0-9_]+/,

  // ╭──────────────────────────────────────────────────────────╮
  // │                        EXEC CICS                         │
  // ╰──────────────────────────────────────────────────────────╯
  exec_cics: ($) =>
    seq(
      seq($._EXEC, kw("CICS"), op(field("operation", $.cics_operation))),
      repeat($._exec_cics_body),
      kw("END-EXEC"),
    ),

  cics_operation: (_) =>
    choice(kw("PUT"), kw("ASSIGN"), kw("GET"), kw("LINK"), kw("DELETE")),

  _exec_cics_body: ($) => prec.right(15, seq($._exec_cics_statements, C($))),
  _exec_cics_statements: ($) =>
    choice(
      field("program", seq(kw("PROGRAM"), paren($.variable))),
      field("commarea", seq(kw("COMMAREA"), paren($.variable))),
      field(
        "container",
        seq(kw("CONTAINER"), paren(choice($.string, $.variable))),
      ),
      field("into", seq($._INTO, paren($.variable))),
      seq($._FROM, paren($.variable)),
      seq(kw("CHANNEL"), paren($.variable)),
      seq(kw("RESP"), paren($.variable)),
      seq(kw("RESP2"), paren($.variable)),
      seq(
        choice($._LENGTH, "FLENGTH"),
        paren(choice(seq($._LENGTH, $._OF, $.variable), $.variable)),
      ),
      kw("RETURN"),
      kw("NOHANDLE"),
      kw("NODATA"),
    ),

  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  _OF: () => kw("OF"),
  _FROM: () => kw("FROM"),
  _FOR: () => kw("FOR"),
  _LENGTH: () => kw("LENGTH"),
  _INTO: () => kw("INTO"),
  SELECT: () => kw("SELECT"),
  INSERT: () => kw("INSERT"),
  UPDATE: () => kw("UPDATE"),
  DELETE: () => kw("DELETE"),
};
