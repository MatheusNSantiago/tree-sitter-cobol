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
        $._exec_sql_statements,
        seq(choice(kw("OPEN"), kw("CLOSE")), $.cursor_name), //
        kw("ROLLBACK"),
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
      seq(
        kw("CURSOR"),
        opseq(
          kw("WITH"),
          choice(
            kw("HOLD"), //
            seq(kw("ROWSET"), kw("POSITIONING")),
          ),
        ),
        $._FOR,
      ),
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
    prec.right(
      1,
      choice(
        field("include", seq(kw("INCLUDE"), $.variable)),
        field("fetch", seq(kw("FETCH"), $.cursor_name)),
        //  ╾───────────────────────────────────────────────────────────────────────────────────╼
        choice(
          $.sql_select, // SELECT
          seq(field("operation", $.UPDATE), field("table", $.tab_name)), // UPDATE
          seq(
            field("operation", $.DELETE),
            $._FROM,
            field("table", $.tab_name),
          ), // DELETE
          $.sql_insert,
          field("values", seq($._VALUES, $._sql_values)),
          field("into", seq($._INTO, $._sql_values)),
          field("set", seq(kw("SET"), sep1($._sql_expression, ","))),
          field(
            "from",
            seq(
              $._FROM,
              choice(
                sep1(
                  seq(field("table", $.tab_name), optional($._WORD)), // alias
                  ",",
                ),
                parenOrNot(repeat1($._exec_sql_statements)),
              ),
            ),
          ),
          field("where", seq(kw("WHERE"), $._sql_expression)),
        ),
        // field(
        //   "for_update",
        //   seq($._FOR, kw("UPDATE"), opseq($._OF, $.sql_list)),
        // ),
        // field(
        //   "limit",
        //   seq(kw("FETCH"), kw("FIRST"), $.number, kw("ROWS"), op(kw("ONLY"))),
        // ),
        $.sql_order_by,
        $.sql_case,
        // $.sql_between_expression,
      ),
    ),

  _select_statement: ($) =>
    parenOrNot(
      seq(
        $.sql_select,
        optional(seq($._INTO, $.sql_select_expression)),
        optional($.from),
      ),
    ),

  sql_select: ($) =>
    seq(
      field("operation", $.SELECT),
      op(kw("DISTINCT")),
      $.sql_select_expression,
    ),

  sql_select_expression: ($) =>
    sep1(
      field("value", $._sql_expression),
      op($._alias), //
    ),

  sql_insert: ($) =>
    seq(
      field("operation", $.INSERT),
      $._INTO,
      field("table", $.tab_name),
      $._sql_insert_values,
    ),

  sql_order_by: ($) =>
    seq(
      kw("ORDER"),
      kw("BY"),
      sep1(seq($.sql_field, op(choice(kw("ASC"), kw("DEC")))), ","),
    ),

  sql_case: ($) =>
    seq(
      kw("CASE"),
      $._WHEN,
      $._sql_expression,
      $._THEN,
      $._sql_expression,
      repeat(
        seq(
          $._WHEN,
          $._sql_expression,
          $._THEN,
          $._sql_expression, //
        ),
      ),
      opseq($._ELSE, $._sql_expression),
      $._END,
    ),

  sql_list: ($) => paren(sep1($._sql_expression, ",")),
  _sql_values: ($) => parenOrNot(sep1(seq(":", $.variable), ",")),

  cursor_name: (_) => /[a-zA-Z0-9_-]+/,

  _sql_insert_values: ($) =>
    seq(
      // optional(alias($._column_list, $.list)),
      paren(sep1($._sql_identifier, ",")),
      // choice(
      //   seq($.keyword_values, comma_list($.list, true)),
      //   $._dml_read, //
      // ),
    ),

  _alias: ($) => seq(op(kw("AS")), field("alias", $.sql_identifier)),

  sql_field: ($) =>
    seq(
      opseq($._sql_identifier, "."), // Alias
      $._sql_identifier,
    ),

  //  ╾───────────────────────────────────────────────────────────────────────────────────╼
  _sql_expression: ($) =>
    prec(
      1,
      choice(
        $.sql_unary_expression,
        $.sql_binary_expression,
        prec(2, paren($._sql_expression)),
        $.sql_list,
        $.sql_field,
      ),
    ),

  sql_unary_expression: ($) =>
    choice(
      seq($._NOT, $._sql_expression), //
    ),

  sql_binary_expression: ($) =>
    prec.left(
      3,
      choice(
        $._tab_expr_compare,
        $._tab_expr_data,
        $._tab_expr_in, //
        seq($._sql_expression, choice($._AND, $._OR), $._sql_expression),
      ),
    ),

  _tab_expr_in: ($) =>
    seq($._tab_expr_calc, kw("IN"), paren(sep1($._tab_expr_data, ","))),

  // sql_between_expression: ($) =>
  //   prec.left(
  //     seq(
  //       seq($._sql_expression, op($._NOT), kw("BETWEEN")),
  //       seq(field("low", $._sql_expression), $._AND, field("high", $._sql_expression)),
  //     ),
  //   ),

  _tab_expr_data: ($) =>
    prec.left(
      choice(
        $.boolean,
        $.number,
        // $.variable,
        $.sql_field,
        seq(":", $.variable),
        $.string,
        $.constant,
      ),
    ),

  _tab_expr_calc: ($) =>
    prec(
      1,
      choice(
        $._tab_expr_calc_binary,
        $._tab_expr_calc_unary,
        $._tab_expr_data,
        seq("(", $._tab_expr_data, ")"),
      ),
    ),

  _tab_expr_calc_binary: ($) =>
    choice(
      prec.left(1, seq($._tab_expr_calc, "+", $._tab_expr_calc)),
      prec.left(1, seq($._tab_expr_calc, "-", $._tab_expr_calc)),
      prec.left(2, seq($._tab_expr_calc, "**", $._tab_expr_calc)),
      prec.left(2, seq($._tab_expr_calc, "*", $._tab_expr_calc)),
      prec.left(2, seq($._tab_expr_calc, "/", $._tab_expr_calc)),
      prec.left(3, seq($._tab_expr_calc, "^", $._tab_expr_calc)),
    ),

  _tab_expr_calc_unary: ($) =>
    prec(
      4,
      choice(
        seq("+", $._tab_expr_calc),
        seq("-", $._tab_expr_calc),
        seq("^", $._tab_expr_calc),
      ),
    ),

  _tab_expr_compare: ($) =>
    prec.left(
      -1,
      seq(
        $._tab_expr_calc,
        $._comparator,
        $._tab_expr_calc, //
      ),
    ),

  tab_name: ($) => seq($._sql_identifier, ".", $._sql_identifier),
  sql_identifier: ($) => $._sql_identifier,
  _sql_identifier: (_) => /[a-zA-Z_][0-9a-zA-Z_]*/,
  sql_identifier: ($) => $._sql_identifier,

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
