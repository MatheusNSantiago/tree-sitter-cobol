module.exports = {
  _statement: ($) =>
    seq(
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
      ),
      C($),
    ),
  ...require("./perform"),
  ...require("./if"),
  ...require("./evaluate"),
  ...require("./display"),
  ...require("./select"),
  ...require("./call"),
  ...require("./open"),
  ...require("./read"),
  ...require("./write"),
  ...require("./search"),

  exit_statement: (_) => seq(kw("EXIT"), "."),
  stop_run_statement: (_) => seq(kw("STOP"), kw("RUN"), "."),
  close_statement: ($) => seq(kw("close"), repeat1($.file_name), "."),
  copy_statement: ($) => seq(kw("COPY"), field("copybook", $.WORD), "."),
  goto_statement: ($) => seq(kw("GO"), $._TO, field("to", $.WORD), o(".")),
  set_statement: ($) => seq(kw("SET"), $.variable, $._TO, $._expr_data, o(".")),
  cancel_statement: ($) => seq(kw("CANCEL"), $.variable, o(".")),
  next_sentence: (_) => seq(kw("NEXT"), kw("SENTENCE"), o(".")),
  add_statement: ($) =>
    seq(kw("ADD"), choice($.variable, $.number), kw("TO"), $.variable, o(".")),
  multiply_statement: ($) =>
    seq(
      kw("MULTIPLY"),
      choice($.variable, $.number),
      kw("BY"),
      $.variable,
      o("."),
    ),
  move_statement: ($) =>
    prec.right(
      seq(
        kw("MOVE"),
        field("from", $._expr_data),
        $._TO,
        field("to", repeat1($.variable)),
        o("."),
      ),
    ),
  exec_sql_statement: ($) =>
    seq(kw("EXEC"), kw("SQL"), repeat($._statement), kw("END-EXEC"), o(".")),

  compute_statement: ($) => seq(kw("COMPUTE"), $.expr, o(".")),

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
      seq(kw("DELIMITED"), optional($._BY), $._SIZE),
    ),
};
