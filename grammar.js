op = (thing) => optional(thing);
C = ($) => repeat(choice($.comment, $._BLANK_LINE));
kw = (keyword) => choice(keyword.toUpperCase(), keyword.toLowerCase());
aspas = (thing) =>
  choice(
    seq(op("'"), thing, op("'")),
    seq(op('"'), thing, op('"')), //
  );

module.exports = grammar({
  name: "cobol",
  word: ($) => $._WORD,
  externals: ($) => [
    $._BLANK_LINE,
    $._WHITE_SPACES,
    $._PREFIX,
    $.paragraph_header,
    $.section_header,
    $._INLINE_COMMENT,
    $._SUFFIX_COMMENT,
  ],
  extras: ($) => [
    /\s/,
    $.comment,
    $._WHITE_SPACES,
    $._BLANK_LINE,
    $._PREFIX,
    $._INLINE_COMMENT,
    $._SUFFIX_COMMENT,
  ],
  rules: {
    source_file: ($) =>
      seq(
        $.identification_division,
        optional($.environment_division),
        optional($.data_division),
        optional($.procedure_division),
      ),

    // http://stackoverflow.com/questions/13014947/regex-to-match-a-c-style-multiline-comment/36328890#36328890
    comment: (_) => token(seq("*", /([^\n])*/)),

    ...require("./src/grammar/division/identification"),
    ...require("./src/grammar/division/environment"),
    ...require("./src/grammar/division/data"),
    ...require("./src/grammar/division/procedure"),
    // ╾───────────────────────────────────────────────────────────────────────────────────╼
    ...require("./src/grammar/keywords"),
    ...require("./src/grammar/base"),
    ...require("./src/grammar/expression"),
    ...require("./src/grammar/statement/"),
  },
});
