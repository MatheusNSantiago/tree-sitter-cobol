module.exports = {
  if_statement: ($) => choice($._if_block, $._if_sentence),
  //  ╭──────────────────────────────────────────────────────────╮
  //  │                           body                           │
  //  ╰──────────────────────────────────────────────────────────╯

  _if_block: ($) =>
    seq(
      seq($._IF, field("condition", $.expr)),
      seq(op($._THEN), field("then", $.statement_block)),
      opseq($._ELSE, field("else", $.statement_block)),
      $._END_IF,
    ),

  //  ╭──────────────────────────────────────────────────────────╮
  //  │                         sentence                         │
  //  ╰──────────────────────────────────────────────────────────╯

  _if_sentence: ($) =>
    prec.right(
      seq(
        seq($._IF, field("condition", $.expr)),
        seq(op($._THEN), field("then", op($.atomic_imperative_block))),
        opseq($._ELSE, field("else", $.atomic_imperative_block)),
      ),
    ),
};
