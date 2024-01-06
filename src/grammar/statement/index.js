module.exports = {
  _statement: ($) =>
    choice(
      $.perform_statement,
      $.if_statement,
      $.evaluate_statement,
      $.display_statement,
      $.exit_statement,
      $.stop_run_statement,
      $.select_statement,
    ),
  ...require("./perform"),
  ...require("./if"),
  ...require("./evaluate"),
  ...require("./display"),
  ...require("./select"),

  exit_statement: (_) => prec(2, seq(kw("EXIT"), ".")),
  stop_run_statement: (_) => seq(kw("STOP"), kw("RUN"), "."),
};
