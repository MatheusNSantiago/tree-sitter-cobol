// Adaptado de https://github.com/DerekStride/tree-sitter-sql

const SQL_PREC = {
  OR: 1, // OR
  AND: 2, // AND
  BETWEEN: 3, // BETWEEN
  LIKE: 4, // LIKE
  IS: 5, // IS NULL, IS NOT NULL
  COMPARE: 6, // =, <, >, IN
  CONCAT: 7, // || (String concatenation)
  ADDITIVE: 8, // +, - (binary)
  MULTIPLICATIVE: 9, // *, /
  UNARY_PLUS_MINUS: 10, // +, - (unary)
  UNARY_NOT: 11, // NOT (unary)
};

module.exports = {
  exec_sql: ($) =>
    prec.right(
      seq(
        alias(seq($._EXEC, $._SQL), $.sql_exec_start_delimiter),
        $._sql_statement,
        alias($._END_EXEC, $.sql_end_exec_delimiter),
      ),
    ),

  sql_with_clause: ($) =>
    seq($._WITH, sep1($.sql_common_table_expression, ",")),

  sql_common_table_expression: ($) =>
    seq(
      field("name", $.sql_identifier),
      optional(paren(sep1($.sql_identifier, ","))),
      seq($._AS, field("query", $.sql_subquery)),
    ),

  _sql_statement: ($) =>
    choice(
      $.sql_select_statement,
      $.sql_insert_statement,
      $.sql_update_statement,
      $.sql_delete_statement,
      $.sql_whenever_statement,
      $.sql_include_statement,
      $.sql_fetch_statement,
      $.sql_declare_statement,
      $.sql_open_statement,
      $.sql_close_statement,
      $.sql_commit_statement,
      $.sql_rollback_statement,
    ),

  sql_line_comment: (_) => token(prec(1, /--[^\n]*/)),
  sql_block_comment: (_) => token(prec(1, /\/\*[^*]*\*+(?:[^/*][^*]*\*+)*\//)),
  sql_identifier: (_) => token(/[a-zA-Z][0-9a-zA-Z_-]*/), // /([0-9][a-zA-Z0-9-]*[a-zA-Z][a-zA-Z0-9-]*)|([a-zA-Z][a-zA-Z0-9-]*)/,
  // sql_identifier: ($) => $._WORD,

  cursor_identifier: ($) => $.sql_identifier,

  sql_select_statement: ($) =>
    seq(
      op($.sql_with_clause),
      $.sql_select_clause,
      op($.sql_into_clause),
      $.sql_from_clause,
      op($.sql_where_clause),
      op($.sql_group_by_clause),
      op($.sql_having_clause),
      op($.sql_order_by_clause),
      op($.sql_limit_clause),
      op($.sql_fetch_first_clause),
      op($.sql_optimize_for_clause),
      op($.sql_for_update_of_clause),
    ),

  sql_select_clause: ($) =>
    seq(kw("SELECT"), op($._DISTINCT), $.sql_select_expression_list),

  sql_into_clause: ($) => seq($._INTO, sep1($.sql_variable, ",")),

  sql_from_clause: ($) =>
    seq($._FROM, sep1($.sql_relation, ","), repeat($.sql_join_clause)),

  sql_where_clause: ($) => seq($._WHERE, field("predicate", $.sql_expression)),

  sql_group_by_clause: ($) => seq($._GROUP, $._BY, sep1($.sql_expression, ",")),

  sql_having_clause: ($) =>
    seq($._HAVING, field("predicate", $.sql_expression)),

  sql_order_by_clause: ($) =>
    seq($._ORDER, $._BY, sep1($.sql_order_target, ",")),

  sql_limit_clause: ($) => seq($._LIMIT, field("limit", $.integer)),

  sql_fetch_first_clause: ($) =>
    seq(
      $._FETCH,
      $._FIRST,
      field("count", choice($.integer, $.sql_variable)),
      choice($._ROWS, $._ROW),
      $._ONLY,
    ),

  sql_optimize_for_clause: ($) =>
    seq(
      $._OPTIMIZE,
      $._FOR,
      field("count", $.integer),
      choice($._ROWS, $._ROW),
    ),

  sql_for_update_of_clause: ($) =>
    seq($._FOR, $._UPDATE, op($._OF), sep1($.sql_identifier, ",")),

  sql_select_expression_list: ($) => sep1($.sql_term, ","),

  sql_term: ($) =>
    seq(
      field(
        "value",
        choice(
          // $.sql_all_fields,
          $.sql_expression, //
        ),
      ),
      op($.sql_alias),
    ),

  // sql_all_fields: ($) =>
  //   seq(
  //     opseq(choice($.sql_identifier, $.sql_object_reference), "."), //
  //     "*",
  //   ),

  sql_alias: ($) => seq(op($._AS), field("alias", $.sql_identifier)),

  sql_relation: ($) =>
    prec.right(
      seq(
        choice($.sql_subquery, $.sql_object_reference, $.sql_invocation),
        op($.sql_alias),
      ),
    ),

  sql_join_clause: ($) =>
    seq(
      op(
        choice(
          $._INNER, //
          seq(choice($._LEFT, $._RIGHT, $._FULL), op($._OUTER)),
        ),
      ),
      $._JOIN,
      $.sql_relation,
      $._ON,
      field("predicate", $.sql_expression),
    ),

  sql_order_target: ($) => seq($.sql_expression, op(choice($._ASC, $._DESC))),

  sql_insert_statement: ($) =>
    seq(
      $._INSERT,
      $._INTO,
      $.sql_object_reference,
      op(alias($.sql_column_list_for_insert, $.sql_columns)),
      choice(
        seq($._VALUES, $.sql_value_list_for_insert),
        $.sql_select_statement, // INSERT INTO ... SELECT ...
      ),
    ),

  sql_column_list_for_insert: ($) => paren(sep1($.sql_identifier, ",")),
  sql_value_list_for_insert: ($) => paren(sep1($.sql_expression, ",")),

  sql_update_statement: ($) =>
    seq(
      $._UPDATE,
      op($._ONLY),
      $.sql_object_reference,
      $._SET,
      $.sql_assignment_list,
      op($.sql_where_clause),
    ),

  sql_assignment_list: ($) => sep1($.sql_assignment, ","), // Using your sep1 helper

  sql_assignment: ($) =>
    seq(
      field("column", $.sql_qualified_field),
      "=",
      field("value", $.sql_expression),
    ),

  sql_delete_statement: ($) =>
    seq(
      kw("DELETE"),
      $._FROM,
      op($._ONLY),
      $.sql_object_reference,
      op($.sql_where_clause),
    ),

  _sql_declare_cursor_body: ($) =>
    seq(
      field("cursor_name", $.sql_identifier),
      op($._SCROLL),
      $._CURSOR,
      repeat($.sql_cursor_option),
      $._FOR,
      field("query", $.sql_select_statement),
    ),

  sql_cursor_option: ($) =>
    seq(
      $._WITH,
      choice(
        $._HOLD, //
        seq($._ROWSET, $._POSITIONING),
      ),
    ),

  sql_whenever_statement: ($) =>
    seq(
      $._WHENEVER,
      field(
        "condition",
        choice(kw("SQLERROR"), kw("SQLWARNING"), seq($._NOT, kw("FOUND"))),
      ),
      field(
        "action",
        choice(
          seq(choice($._GO, $._GOTO), $._TO, field("paragraph_name", $.WORD)),
          $._CONTINUE,
          $._STOP,
        ),
      ),
    ),

  sql_include_statement: ($) =>
    seq(
      $._INCLUDE,
      field("member_name", choice(kw("SQLCA"), kw("SQLDA"), $.sql_identifier)),
    ),

  sql_fetch_statement: ($) =>
    seq(
      $._FETCH,
      optional(
        choice(
          seq($._NEXT, $._ROWSET, op($._FROM)),

          //FETCH ROWSET STARTING AT ABSOLUTE :DEBSBS05-PSC
          seq(
            $._ROWSET,
            $._STARTING,
            $._AT,
            $._ABSOLUTE,
            field("start", $.sql_variable),
          ),
        ),
      ),
      field("cursor_name", $.cursor_identifier),
      opseq($._FOR, field("count", $.integer), $._ROWS),
      $._INTO,
      sep1($.sql_variable, ","),
    ),

  sql_open_statement: ($) =>
    seq($._OPEN, field("cursor_name", $.cursor_identifier)),

  sql_close_statement: ($) =>
    seq($._CLOSE, field("cursor_name", $.cursor_identifier)),

  sql_commit_statement: ($) => $._COMMIT,

  sql_rollback_statement: ($) => $._ROLLBACK,

  sql_declare_statement: ($) =>
    seq(
      $._DECLARE,
      choice(
        alias($._sql_declare_cursor_body, $.sql_declare_cursor_statement),
        alias($._sql_declare_table_body, $.sql_declare_table_statement),
      ),
    ),

  _sql_declare_table_body: ($) =>
    seq(
      field("table_name", $.sql_object_reference), // e.g., DB2DEB.TDEB1120
      $._TABLE,
      paren(sep1($.sql_column_definition, ",")),
    ),

  sql_column_definition: ($) =>
    seq(
      field("column_name", $.sql_identifier),
      field("data_type", $.sql_data_type),
      op(repeat1($.sql_column_constraint)),
    ),

  sql_data_type: ($) =>
    choice(
      $._SMALLINT,
      $._INTEGER,
      seq($._DECIMAL, op(paren(sep1($.integer, ",")))), // DECIMAL(p, s)
      seq($._NUMERIC, op(paren(sep1($.integer, ",")))), // NUMERIC(p, s)
      seq($._CHARACTER, op(paren($.integer))),
      seq($._CHAR, op(paren($.integer))),
      seq($._VARCHAR, paren($.integer)),
      $._DATE,
      $._TIME,
      $._TIMESTAMP,
    ),

  sql_column_constraint: ($) => choice(seq($._NOT, kw("NULL"))),

  sql_expression: ($) =>
    prec(
      1,
      choice(
        $.sql_literal,
        alias($.sql_qualified_field, $.sql_column_reference),
        $.sql_variable,
        $.sql_parenthesized_expression,
        $.sql_unary_expression,
        $.sql_binary_expression,
        $.sql_case_expression,
        prec(2, $.sql_invocation),
        $.sql_list,
        $.sql_subquery,
        $.sql_exists_expression,
        $.sql_between_expression,
        $.sql_like_expression,
        $.sql_current_of_cursor,
        $.sql_all_fields,
      ),
    ),

  sql_parenthesized_expression: ($) => prec(2, paren($.sql_expression)),

  sql_unary_expression: ($) =>
    choice(
      prec.left(
        SQL_PREC.UNARY_NOT,
        seq(field("operator", $._NOT), field("operand", $.sql_expression)),
      ),
      prec.left(
        SQL_PREC.UNARY_PLUS_MINUS,
        seq(
          field("operator", choice("-", "+")),
          field("operand", $.sql_expression),
        ),
      ),
    ),

  sql_binary_expression: ($) => {
    const operators = [
      // Arithmetic
      ["*", SQL_PREC.MULTIPLICATIVE],
      ["/", SQL_PREC.MULTIPLICATIVE],
      ["+", SQL_PREC.ADDITIVE],
      ["-", SQL_PREC.ADDITIVE],
      ["||", SQL_PREC.CONCAT],
      // Comparison
      ["=", SQL_PREC.COMPARE],
      ["<", SQL_PREC.COMPARE],
      ["<=", SQL_PREC.COMPARE],
      ["!=", SQL_PREC.COMPARE],
      ["<>", SQL_PREC.COMPARE],
      [">=", SQL_PREC.COMPARE],
      [">", SQL_PREC.COMPARE],
      // IS
      [$._IS, SQL_PREC.IS],
      [seq($._IS, $._NOT), SQL_PREC.IS],
      // Logical
      [$._AND, SQL_PREC.AND],
      [$._OR, SQL_PREC.OR],
    ];
    return choice(
      ...operators.map(([operator, precedence]) =>
        prec.left(
          precedence,
          seq(
            field("left", $.sql_expression),
            op($._NOT),
            field("operator", operator),
            field("right", $.sql_expression),
          ),
        ),
      ),
      prec.left(
        SQL_PREC.COMPARE,
        seq(
          field("left", $.sql_expression),
          op($._NOT),
          kw("IN"),
          field("right", choice($.sql_list, $.sql_subquery)),
        ),
      ),
    );
  },

  sql_like_expression: ($) =>
    prec.left(
      SQL_PREC.LIKE,
      seq(
        field("left", $.sql_expression),
        op($._NOT),
        kw("LIKE"),
        field("pattern", $.sql_expression),
      ),
    ),

  sql_between_expression: ($) =>
    prec.left(
      SQL_PREC.BETWEEN,
      seq(
        field("subject", $.sql_expression),
        op($._NOT),
        kw("BETWEEN"),
        field("low", $.sql_expression),
        $._AND,
        field("high", $.sql_expression),
      ),
    ),

  sql_case_expression: ($) =>
    seq(
      kw("CASE"),
      op(field("input_expression", $.sql_expression)),
      repeat1(
        seq(
          $._WHEN,
          field("condition", $.sql_expression),
          $._THEN,
          field("result", $.sql_expression),
        ),
      ),
      op(seq($._ELSE, field("else_result", $.sql_expression))),
      $._END,
    ),

  sql_exists_expression: ($) => seq(kw("EXISTS"), $.sql_subquery),

  sql_subquery: ($) => paren($.sql_select_statement),

  sql_list: ($) => paren(sep1($.sql_expression, ",")),

  sql_literal: ($) =>
    choice($.sql_string_literal, $.sql_constant, $.number, $._NULL),

  sql_all_fields: (_) => "*",

  sql_string_literal: (_) => seq(/'([^']|'')*'/, repeat(/'([^']|'')*'/)),

  sql_constant: ($) =>
    choice(
      kw("CURRENT_TIME"),
      kw("CURRENT_DATE"),
      seq($._CURRENT, choice($._TIME, $._DATE, $._TIMESTAMP)), //
    ),

  sql_variable: ($) =>
    seq(
      field("value_var", seq(":", $.variable)),
      op(field("indicator_var", seq(":", $.variable))),
    ),

  sql_qualified_field: ($) =>
    seq(op(seq($.sql_object_reference, ".")), field("name", $.sql_identifier)),

  sql_function_name: ($) =>
    choice(
      $.sql_identifier,
      $._VALUE,
      $._CHAR,
      $._TO_CHAR, //
      $._TO_DATE, //
    ),

  sql_invocation: ($) =>
    seq(
      field("function_name", $.sql_function_name),
      paren(op(sep1(seq(op($._DISTINCT), $.sql_expression), ","))),
    ),

  sql_object_reference: ($) =>
    prec.right(
      choice(
        seq(
          field("schema", $.sql_identifier),
          ".",
          field("name", $.sql_identifier),
        ),
        field("name", $.sql_identifier),
      ),
    ),

  sql_current_of_cursor: ($) =>
    seq($._CURRENT, $._OF, field("cursor_name", $.cursor_identifier)),

  _ROWSET: (_) => kw("ROWSET"),
  _FETCH: (_) => kw("FETCH"),
  _DECLARE: (_) => kw("DECLARE"),
};
