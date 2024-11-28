module.exports = {
  call_statement: ($) =>
    seq(
      kw("CALL"),
      field("program", aspas($.program)),
      field("using", optional($.using)),
    ),
  program: ($) => $._WORD,
  using: ($) => seq(kw("USING"), repeat1($.variable)),
};
