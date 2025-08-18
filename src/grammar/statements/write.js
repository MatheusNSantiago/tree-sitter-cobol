module.exports = {
  write_statement: ($) =>
    seq(
      kw("WRITE"),
      field("record_name", $.variable),
      opseq($._FROM, field("from", $.variable)),
      op(kw("END-WRITE")),
    ),
};
