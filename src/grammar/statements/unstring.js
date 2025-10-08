module.exports = {
  unstring_statement: ($) =>
    prec.right(
      seq(
        $._UNSTRING,
        field("text", $.variable),
        optional($.unstring_delimited),
        $._INTO,
        // repeat1($.unstring_into_clause),
        repeat1(field("value", $.variable)),
        // seq(optional($._WITH), $._POINTER, $.variable),
        // optional($.unstring_tallying),
        $._END_UNSTRING,
      ),
    ),

  _delimiter: ($) => choice($.variable, $.string, $.constant),

  unstring_delimited: ($) =>
    seq($._DELIMITED, optional($._BY), repeat1($.unstring_delimited_item)),

  unstring_delimited_item: ($) =>
    seq(op($._ALL), field("delimiter", $._delimiter)),

  unstring_into_clause: ($) =>
    seq(
      field("value", $.variable),
      optional(
        seq($._DELIMITER, optional($._IN), field("delimiter", $._delimiter)),
      ),
      optional(seq($._COUNT, optional($._IN), field("count", $.variable))),
    ),

  unstring_tallying: ($) =>
    seq($._TALLYING, optional($._IN), field("tallying", $.variable)),
};
