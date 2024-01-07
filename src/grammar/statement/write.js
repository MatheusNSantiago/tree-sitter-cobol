module.exports = {
  write_statement: ($) =>
    seq(
      kw("WRITE"),
      field("record_name", $.variable),
      optional(seq("FROM", field("from", $.variable))),
      optional("."),
    ),
};
