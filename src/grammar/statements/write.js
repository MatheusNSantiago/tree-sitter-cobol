module.exports = {
  write_statement: ($) =>
    seq(
      kw("WRITE"),
      field("record_name", $.variable),
      op(seq("FROM", field("from", $.variable))),
    ),
};
