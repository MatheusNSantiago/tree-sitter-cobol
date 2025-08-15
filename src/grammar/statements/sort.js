module.exports = {
  sort_statement: ($) =>
    seq(
      kw("SORT"),
      seq(
        field("x", $.variable),
        field("keys", repeat($.sort_key)),
        field(
          "duplicates",
          optional(
            seq(optional($._WITH), kw("DUPLICATES"), op($._IN), $._ORDER),
          ),
        ),
        // field(
        //   "collating",
        //   optional(seq($._coll_sequence, optional($._IS), $.qualified_word)),
        // ),
        field(
          "input",
          optional(
            choice(
              $.sort_input_using,
              $.sort_input_procedure, //
            ),
          ),
        ),
        field(
          "output",
          optional(
            choice(
              $.sort_output_giving,
              $.sort_output_procedure, //
            ),
          ),
        ),
      ),
    ),

  sort_key: ($) =>
    seq(
      op(kw("ON")),
      field("order", choice($._ASCENDING, $._DESCENDING)),
      seq(op($._KEY), op($._IS)),
      field("keys", repeat($.variable)),
    ),

  sort_input_using: ($) => seq($._USING, repeat1($.file_name)),

  sort_input_procedure: ($) =>
    seq($._INPUT, $._PROCEDURE, optional($._IS), $.perform_simple),

  sort_output_giving: ($) => seq($._GIVING, repeat1($.file_name)),

  sort_output_procedure: ($) =>
    seq($._OUTPUT, $._PROCEDURE, optional($._IS), $.perform_simple),
};
