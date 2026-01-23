module.exports = {
  read_statement: ($) => choice($._read_block, $._read_sentence),

  // AT END e NOT AT END consolidados em regex
  _AT_END: (_) => /[aA][tT][ \t]+[eE][nN][dD]/,
  _NOT_AT_END: (_) => /[nN][oO][tT][ \t]+[aA][tT][ \t]+[eE][nN][dD]/,

  //  ╭──────────────────────────────────────────────────────────╮
  //  │                          BLOCO                           │
  //  ╰──────────────────────────────────────────────────────────╯
  _read_block: ($) =>
    seq(
      seq($._READ, field("file", $.file_name)),
      opseq($._INTO, field("record", $.variable)),
      repeat(
        choice(
          alias($._at_end_block, $.at_end_block),
          alias($._not_at_end_block, $.not_at_end_block),
        ),
      ),
      $._END_READ,
    ),

  _at_end_block: ($) => seq($._AT_END, $.statement_block),
  _not_at_end_block: ($) => seq($._NOT_AT_END, $.statement_block),

  //  ╭──────────────────────────────────────────────────────────╮
  //  │                         SENTENÇA                         │
  //  ╰──────────────────────────────────────────────────────────╯
  _read_sentence: ($) =>
    prec.left(
      seq(
        seq($._READ, field("file", $.file_name)),
        opseq($._INTO, field("record", $.variable)),
        repeat(
          choice(
            alias($._at_end_sentence, $.at_end_block),
            alias($._not_at_end_sentence, $.not_at_end_block),
          ),
        ),
      ),
    ),

  _at_end_sentence: ($) => seq($._AT_END, $.atomic_imperative_block),
  _not_at_end_sentence: ($) => seq($._NOT_AT_END, $.atomic_imperative_block),
};
