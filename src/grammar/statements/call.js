module.exports = {
  call_statement: ($) =>
    seq(
      kw("CALL"),
      field("program", choice($.variable, $.string)),
      field("using", opseq($._USING, repeat1($.variable))),
    ),
};
