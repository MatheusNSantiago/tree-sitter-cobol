module.exports = {
  _statement: ($) => prec.right(15, seq($._statements, C($))),
  _statements: ($) =>
    prec.right(seq(
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
        $._exec_statement,
        $.compute_statement,
        $.string_statement,
        $.initialize_statement,
      ),
      op("."),
    )),

  ...require("./perform"),
  ...require("./if"),
  ...require("./evaluate"),
  ...require("./select"),
  ...require("./call"),
  ...require("./read"),
  ...require("./write"),
  ...require("./search"),
  ...require("./compute"),
  ...require("./exec"),

  exit_statement: (_) => seq(kw("EXIT"), "."),
  stop_run_statement: (_) => seq(kw("STOP"), kw("RUN"), "."),
  close_statement: ($) => seq(kw("close"), repeat1($.file_name)),
  copy_statement: ($) => seq(kw("COPY"), field("copybook", $.variable), "."),
  goto_statement: ($) => seq(kw("GO"), $._TO, field("to", $.WORD)),
  set_statement: ($) =>
    seq(
      kw("SET"),
      $.variable,
      choice($._TO, seq($._UP, $._BY)),
      seq($._expr_data),
    ),
  cancel_statement: ($) => seq(kw("CANCEL"), $.variable),
  next_sentence: (_) => seq(kw("NEXT"), kw("SENTENCE")),
  add_statement: ($) =>
    seq(kw("ADD"), choice($.variable, $.number), kw("TO"), $.variable),
  multiply_statement: ($) =>
    seq(
      kw("MULTIPLY"),
      choice($.variable, $.number),
      kw("BY"),
      $.variable,
    ),
  move_statement: ($) =>
    seq(
      kw("MOVE"),
      field("from", seq(op(seq(kw("LENGTH"), $._OF)), $._expr_data)),
      $._TO,
      field("to", repeat1($.variable)),
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
  display_statement: ($) => seq(kw("DISPLAY"), repeat1($._expr_data)),

  open_statement: ($) =>
    seq(
      kw("OPEN"),
      repeat1(
        seq(
          choice(kw("INPUT"), kw("OUTPUT")), //
          repeat1($.file_name),
        ),
      ),
    ),

  initialize_statement: ($) =>
    seq(
      kw("INITIALIZE"), //
      repeat1($.variable),
    ),
};
