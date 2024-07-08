module.exports = {
  // ╭──────────────────────────────────────────────────────────╮
  // │                       IF STATEMENT                       │
  // ╰──────────────────────────────────────────────────────────╯

  _IF: (_) => kw("IF"),
  _THEN: (_) => kw("THEN"),
  _ELSE: (_) => kw("ELSE"),
  _END_IF: (_) => kw("END-IF"),

  if_statement: ($) => choice($._if_inline, $._if_block),

  _if_inline: ($) =>
    prec(
      16,
      seq($._IF, field("condition", $.expr), op($._THEN), $._statements),
    ),

  _if_block: ($) =>
    prec.right(
      seq(
        $._if,
        // repeat($._else_if),
        optional($._else),
        $._END_IF,
        op("."),
      ),
    ),

  _if: ($) =>
    seq(
      seq($._IF, field("condition", $.expr), op($._THEN)),
      C($),
      repeat($._statement),
    ),

  // _else_if: ($) =>
  //     seq(
  //       seq($._ELSE, $._IF),
  //       field("condition", choice($.expr)),
  //       op($._THEN),
  //       C($),
  //       repeat($._statement),
  //     ),

  _else: ($) => prec.right(15, seq($._ELSE, C($), repeat($._statement))),
};
