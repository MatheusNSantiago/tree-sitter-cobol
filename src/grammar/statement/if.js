module.exports = {
  // ╭──────────────────────────────────────────────────────────╮
  // │                       IF STATEMENT                       │
  // ╰──────────────────────────────────────────────────────────╯

  _IF: (_) => kw("IF"),
  _THEN: (_) => kw("THEN"),
  _ELSE: (_) => kw("ELSE"),
  _END_IF: (_) => kw("END-IF"),

  if_statement: ($) =>
    prec.right(
      2,
      seq(
        $._IF,
        field("condition", $.expr),
        op($._THEN),
        C($),
        repeat($._statement),
        repeat(
          seq(
            seq($._ELSE, $._IF),
            field("condition", choice($.expr)),
            op($._THEN),
            C($),
            repeat($._statement),
          ),
        ),
        optional(seq($._ELSE, C($), repeat($._statement))),
        $._END_IF,
        op("."),
      ),
    ),
};
