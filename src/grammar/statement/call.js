module.exports = {
  call_statement: ($) =>
    seq(
      kw("CALL"),
      field("program_name", $.program_name),
      optional(field("using", $.using)),
      ".",
    ),
  program_name: ($) => $._WORD,

  using: ($) => seq(kw("USING"), repeat1($.variable)),
};
