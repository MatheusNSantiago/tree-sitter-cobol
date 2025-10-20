module.exports = {
  // Mantemos "if_statement" como está (bloco e sentence internos) para uso interno,
  // mas o fechamento com ponto será feito por "if_sentence_statement" abaixo.
  if_statement: ($) =>
    choice(prec.dynamic(2, $._if_sentence), prec.dynamic(1, $._if_block)),

  //  ╭──────────────────────────────────────────────────────────╮
  //  │                           body                           │
  //  ╰──────────────────────────────────────────────────────────╯

  _if_block: ($) =>
    prec.right(
      1,
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

  _if_sentence: ($) =>
    prec.right(
      10, // Precedência mais alta para associar o ELSE ao IF mais interno.
      seq(
        seq($._IF, field("condition", $.expr)),
        seq(op($._THEN), field("then", $._if_sentence_body)),
        opseq($._ELSE, field("else", $._if_sentence_body)),
      ),
    ),

  _if_sentence_body: ($) =>
    prec.right(
      5,
      choice(
        // Tente primeiro um IF sentence aninhado; depois, apenas imperativos.
        seq($._if_sentence, op($.atomic_imperative_block)),
        $.atomic_imperative_block,
      ),
    ),
};
