module.exports = {
  identification_division: ($) =>
    seq(
      field("division_header", $.identification_division_header),
      $.program_id,
      optional($.author),
      C($),
    ),

  identification_division_header: (_) =>
    seq(kw("IDENTIFICATION"), kw("DIVISION"), "."),

  program_id: ($) => seq(kw("PROGRAM-ID"), ".", $.program_name, "."),
  program_name: ($) => $._WORD,

  author: ($) => seq(kw("AUTHOR"), ".", $.author_name, "."),
  author_name: ($) => $._WORD,
};
