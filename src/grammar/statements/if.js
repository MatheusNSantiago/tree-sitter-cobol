module.exports = {
  if_statement: ($) =>
    prec.right(
      seq(
        seq($._IF, field("condition", $.expr)),
        seq(op($._THEN), field("then", $.statement_block)),
        opseq($._ELSE, field("else", $.statement_block)),
        op($._END_IF),
      ),
    ),
};
