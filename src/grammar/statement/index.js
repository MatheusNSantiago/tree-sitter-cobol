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
  ...require("./close"),

  exit_statement: (_) => prec(2, seq(kw("EXIT"), ".")),
  stop_run_statement: (_) => seq(kw("STOP"), kw("RUN"), "."),
};
