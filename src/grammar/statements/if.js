module.exports = {
  // if_statement: ($) => choice(prec(1, $.if_block), $.if_sentence),

  //  ╭──────────────────────────────────────────────────────────╮
  //  │                           body                           │
  //  ╰──────────────────────────────────────────────────────────╯
  if_block: ($) =>
    prec.right(
      seq(
        seq($._IF, field("condition", $.expr)),
        seq(op($._THEN), field("then", $.statement_block)),
        opseq($._ELSE, field("else", $.statement_block)),
        $._END_IF,
      ),
    ),

  //  ╭──────────────────────────────────────────────────────────╮
  //  │                         sentence                         │
  //  ╰──────────────────────────────────────────────────────────╯
  if_sentence: ($) =>
    prec.right(
      seq(
        seq($._IF, field("condition", $.expr)),
        seq(op($._THEN), field("then", $.atomic_imperative_block)),
        opseq($._ELSE, field("else", $.atomic_imperative_block))
      ),
    ),
};
