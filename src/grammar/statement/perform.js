module.exports = {
  perform_statement: ($) => choice($.perform_simple),

  perform_simple: ($) =>
    seq(
      kw("PERFORM"),
      field("label", $.section_name),
      optional(seq($._THRU, $.section_name)),
      optional("."),
    ),
};
