module.exports = {
  _statement: ($) =>
    choice(
      $.perform_statement,
      $.if_statement,
      $.evaluate_statement,
      $.display_statement,
      $.exit_statement,
      $.goback_statement,
      $.stop_run_statement,
      $.select_statement,
      $.call_statement,
      $.open_statement,
      $.close_statement,
      $.copy_statement,
      $.read_statement,
      $.add_statement,
      $.subtract_statement,
      $.move_statement,
      $.goto_statement,
      $.write_statement,
      $.set_statement,
      $.search_statement,
      $.cancel_statement,
      $.next_sentence,
      $.multiply_statement,
      $.divide_statement,
      $.exec_cics,
      $.exec_sql,
      $.compute_statement,
      $.string_statement,
      $.initialize_statement,
      $.sort_statement,
    ),

  // _statement: ($) => prec.right(15, seq($._statements, C($))),
  // _statements: ($) =>
  //   prec.right(
  //     seq(
  //       choice(
  //         $.perform_statement,
  //         $.if_statement,
  //         $.evaluate_statement,
  //         $.display_statement,
  //         $.exit_statement,
  //         $.goback_statement,
  //         $.stop_run_statement,
  //         $.select_statement,
  //         $.call_statement,
  //         $.open_statement,
  //         $.close_statement,
  //         $.copy_statement,
  //         $.read_statement,
  //         $.add_statement,
  //         $.subtract_statement,
  //         $.move_statement,
  //         $.goto_statement,
  //         $.write_statement,
  //         $.set_statement,
  //         $.search_statement,
  //         $.cancel_statement,
  //         $.next_sentence,
  //         $.multiply_statement,
  //         $.divide_statement,
  //         $.exec_cics,
  //         $.exec_sql,
  //         $.compute_statement,
  //         $.string_statement,
  //         $.initialize_statement,
  //       ),
  //       op("."),
  //     ),
  //   ),

  ...require("./perform"),
  ...require("./if"),
  ...require("./evaluate"),
  ...require("./select"),
  ...require("./call"),
  ...require("./read"),
  ...require("./write"),
  ...require("./search"),
  ...require("./math_statements"),
  ...require("./exec_sql"),
  ...require("./exec_cics"),
  ...require("./move"),
  ...require("./sort"),

  exit_statement: (_) => seq(kw("EXIT"), "."),
  goback_statement: (_) => kw("GOBACK"),
  stop_run_statement: (_) => seq(kw("STOP"), kw("RUN")),
  close_statement: ($) => seq(kw("close"), repeat1($.file_name)),
  copy_statement: ($) => seq(kw("COPY"), field("copybook", $.variable), "."),
  goto_statement: ($) => seq(kw("GO"), optional($._TO), field("to", $.WORD)),
  set_statement: ($) =>
    seq(
      kw("SET"),
      $.variable,
      choice(
        $._TO,
        seq(choice($._UP, kw("DOWN")), $._BY), //
      ),
      seq($._expr_data),
    ),

  cancel_statement: ($) => seq(kw("CANCEL"), $.variable),
  next_sentence: (_) => seq(kw("NEXT"), kw("SENTENCE")),

  string_statement: ($) =>
    seq(
      kw("STRING"),
      field("from", repeat1($.string_item)),
      $._INTO,
      field("into", $.variable),
      op(kw("END-STRING")),
    ),
  string_item: ($) =>
    seq(
      choice($.string, $.variable),
      opseq(kw("DELIMITED"), op($._BY), $._SIZE),
    ),
  display_statement: ($) => seq(kw("DISPLAY"), repeat1($._expr_data)),

  open_statement: ($) =>
    seq(
      kw("OPEN"),
      repeat1(
        choice(
          field("input", seq($._INPUT, repeat1($.file_name))),
          field("output", seq(kw("OUTPUT"), repeat1($.file_name))),
        ),
      ),
    ),

  initialize_statement: ($) =>
    seq(
      kw("INITIALIZE"),
      field("variable", repeat1($.variable)),
      field(
        "replacing",
        op(seq(kw("REPLACING"), repeat1($.initialize_replacing_item))),
      ),
    ),

  initialize_replacing_item: ($) =>
    seq(
      field("category", $._initialize_category),
      $._BY,
      field("by", $._value),
    ),

  _initialize_category: (_) =>
    choice(
      kw("ALPHABETIC"),
      kw("ALPHANUMERIC"),
      kw("NUMERIC"),
      kw("ALPHANUMERIC_EDITED"),
      kw("NUMERIC_EDITED"),
      kw("NATIONAL"),
      kw("NATIONAL_EDITED"),
    ),
};
