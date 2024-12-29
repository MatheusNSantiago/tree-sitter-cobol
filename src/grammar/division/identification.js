module.exports = {
  identification_division: ($) =>
    seq(
      field("division_header", $.identification_division_header),
      C($),
      $.program_id,
      opseq($.author, C($)),
      opseq($.date_written, C($)),
    ),

  identification_division_header: (_) =>
    seq(kw("IDENTIFICATION"), kw("DIVISION"), "."),

  program_id: ($) => seq(kw("PROGRAM-ID"), ".", $.program_name, op(".")),
  program_name: ($) => $._WORD,

  author: ($) => seq(C($), kw("AUTHOR"), ".", $.author_name, op(".")),
  author_name: ($) => repeat1($._WORD),

  date_written: ($) =>
    seq(kw("DATE-WRITTEN"), ".", $.date_written_text, op(".")),

  date_written_text: (_) => /[A-Z\/\d]+/,
};
