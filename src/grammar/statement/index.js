module.exports = {
  _statement: ($) => prec.right(15, seq($._statements, C($))),
  _statements: ($) =>
    choice(
      $.perform_statement,
      $.if_statement,
      $.evaluate_statement,
      $.display_statement,
      $.exit_statement,
      $.stop_run_statement,
      $.select_statement,
      $.call_statement,
      $.open_statement,
      $.close_statement,
      $.copy_statement,
      $.read_statement,
      $.add_statement,
      $.move_statement,
      $.goto_statement,
      $.write_statement,
      $.set_statement,
      $.search_statement,
      $.cancel_statement,
      $.next_sentence,
      $.multiply_statement,
      $.exec_sql_statement,
      $.compute_statement,
      $.string_statement,
      $.initialize_statement,
    ),

  ...require("./perform"),
  ...require("./if"),
  ...require("./evaluate"),
  ...require("./select"),
  ...require("./call"),
  ...require("./read"),
  ...require("./write"),
  ...require("./search"),
  ...require("./compute"),

  exit_statement: (_) => seq(kw("EXIT"), "."),
  stop_run_statement: (_) => seq(kw("STOP"), kw("RUN"), "."),
  close_statement: ($) => seq(kw("close"), repeat1($.file_name), op(".")),
  copy_statement: ($) => seq(kw("COPY"), field("copybook", $.variable), "."),
  goto_statement: ($) => seq(kw("GO"), $._TO, field("to", $.WORD), op(".")),
  set_statement: ($) =>
    seq(
      kw("SET"),
      $.variable,
      choice($._TO, seq($._UP, $._BY)),
      seq($._expr_data, op(".")),
    ),
  cancel_statement: ($) => seq(kw("CANCEL"), $.variable, op(".")),
  next_sentence: (_) => seq(kw("NEXT"), kw("SENTENCE"), op(".")),
  add_statement: ($) =>
    seq(kw("ADD"), choice($.variable, $.number), kw("TO"), $.variable, op(".")),
  multiply_statement: ($) =>
    seq(
      kw("MULTIPLY"),
      choice($.variable, $.number),
      kw("BY"),
      $.variable,
      op("."),
    ),
  move_statement: ($) =>
    seq(
      kw("MOVE"),
      field("from", seq(op(seq(kw("LENGTH"), $._OF)), $._expr_data)),
      $._TO,
      field("to", repeat1($.variable)),
      op("."),
    ),
  exec_sql_statement: ($) =>
    seq(
      seq(kw("EXEC"), kw("SQL")), //
      repeat($._statement),
      kw("END-EXEC"),
      op("."),
    ),

  string_statement: ($) =>
    seq(
      kw("STRING"),
      field("from", repeat1($.string_item)),
      $._INTO,
      field("into", $.variable),
    ),
  string_item: ($) =>
    choice(
      choice($.string, $.variable),
      seq(kw("DELIMITED"), op($._BY), $._SIZE),
    ),
  display_statement: ($) => seq(kw("DISPLAY"), repeat1($._expr_data), op(".")),

  open_statement: ($) =>
    seq(
      kw("OPEN"),
      repeat1(
        seq(
          choice(kw("INPUT"), kw("OUTPUT")), //
          repeat1($.file_name),
        ),
      ),
      op("."),
    ),

  initialize_statement: ($) =>
    seq(
      kw("INITIALIZE"), //
      repeat1($.variable),
      op("."),
    ),
};
