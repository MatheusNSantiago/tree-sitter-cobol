module.exports = {
  write_statement: ($) =>
    seq(
      kw("WRITE"),
      field("record_name", $.variable),
      seq("FROM", field("from", $.variable)),
      optional("."),
    ),
};
