module.exports = {
  _statement: ($) => choice(
    $.perform_statement,
    $.if_statement,
    $.evaluate_statement,
    $.display_statement,
  ),
    ...require("./perform"),
    ...require("./if"),
    ...require("./evaluate"),
    ...require("./display"),
}
