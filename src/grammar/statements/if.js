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
      seq(
        seq($._IF, field("condition", $.expr), op($._THEN)),
        $.then_body,
        opseq($._ELSE, $.else_body),
        op($._END_IF),
      ),
    ),

  then_body: ($) => prec.right(repeat1(choice($._CONTINUE, $._statement))),
  else_body: ($) => prec.right(repeat1(choice($._CONTINUE, $._statement))),
};
