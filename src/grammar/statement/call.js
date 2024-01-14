module.exports = {
  call_statement: ($) =>
    seq(
      kw("CALL"),
      field("program_name", $.program_name),
      field("using", optional($.using)),
      op("."),
    ),
  program_name: ($) => $._WORD,
  using: ($) => seq(kw("USING"), repeat1($.variable)),
};
