module.exports = {
  identification_division: ($) =>
    seq(
      field("division_header", $.identification_division_header),
      C($),
      $.program_id,
      repeat(
        seq(
          choice(
            $.author,
            $.remarks, //
            $.date_written, //
            $.date_compiled, //
          ),
          C($),
        ),
      ),
    ),

  identification_division_header: (_) =>
    seq(kw("IDENTIFICATION"), kw("DIVISION"), "."),

  program_id: ($) => seq(kw("PROGRAM-ID"), ".", $.program_name, op(".")),
  program_name: ($) => $._WORD,

  author: ($) => seq(kw("AUTHOR"), ".", $.author_name, op(".")),
  author_name: ($) => repeat1($._WORD),

  date_written: ($) =>
    seq(kw("DATE-WRITTEN"), ".", opseq(field("date", $.date_text), ".")),

  date_compiled: ($) =>
    seq(kw("DATE-COMPILED"), ".", opseq(field("date", $.date_text), ".")),

  remarks: (_) => seq(kw("REMARKS"), "."),

  date_text: (_) => /[A-Z\/\d]+/,
};
