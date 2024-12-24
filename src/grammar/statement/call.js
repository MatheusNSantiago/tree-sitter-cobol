module.exports = {
  call_statement: ($) =>
    seq(
      kw("CALL"),
      field("program", aspas($.WORD)),
      field("using", opseq(kw("USING"), repeat1($.variable))),
    ),
};
