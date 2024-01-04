C = ($) => repeat(choice($.comment, $._WHITE_SPACES));
sepBy = (pattern, separator) => seq(pattern, repeat(seq(separator, pattern)));
nonempty = (pattern1, pattern2) =>
  choice(seq(pattern1, optional(pattern2)), pattern2);

module.exports = grammar({
  name: "cobol",
  externals: ($) => [
    $.comment,
    $._WHITE_SPACES,
    $._PREFIX_COMMENT,
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
    source_file: ($) =>
      prec.right(
        seq(
          optional($.procedure_division),
        ),
      ),

    ...require("./src/grammar/division/procedure"),
    ...require("./src/grammar/keywords"),
    ...require("./src/grammar/base"),
    ...require("./src/grammar/expression"),
    ...require("./src/grammar/statement/"),
  },
});
