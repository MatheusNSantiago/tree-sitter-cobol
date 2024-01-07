module.exports = {
  add_statement: ($) =>
    seq(
      kw("ADD"),
      field("from", choice($.variable, $.number)),
      kw("TO"),
      field("to", $.variable),
    ),
};
