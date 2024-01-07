C = ($) => repeat(choice($.comment, $._WHITE_SPACES));

module.exports = grammar({
  name: "cobol",
  externals: ($) => [
    $.comment,
    $._WHITE_SPACES,
    $._PREFIX_COMMENT,
    $.PARAGRAPH_HEADER,
    $._INLINE_COMMENT,
    $._SUFFIX_COMMENT,
  ],
  extras: ($) => [
    " ",
    "\n",
    $.comment,
    $._WHITE_SPACES,
    $._PREFIX_COMMENT,
    $._INLINE_COMMENT,
    $._SUFFIX_COMMENT,
  ],
  rules: {
    source_file: ($) => seq(
      $.identification_division,
      optional($.environment_division),
      optional($.data_division),
      optional($.procedure_division),
      // $.procedure_division,
    ),


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










