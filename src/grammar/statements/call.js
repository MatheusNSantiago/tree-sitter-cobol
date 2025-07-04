module.exports = {
  call_statement: ($) =>
    seq(
      kw("CALL"),
      field("program", choice($.variable, $.string)),
      field("using", opseq(kw("USING"), repeat1($.variable))),
    ),
};
