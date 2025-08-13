module.exports = {
  sort_statement: ($) => seq($._SORT, $._sort_body),

  _sort_body: ($) =>
    prec.right(
      seq(
        field("file", $.file_name),
        field("sort", repeat($.sort_key)),
        field("using", op($.sort_using)),
        field("output", op($.sort_giving)),
      ),
    ),

  sort_using: ($) => seq($._USING, repeat1($.file_name)),
  sort_giving: ($) => seq($._GIVING, repeat1($.file_name)),
};
