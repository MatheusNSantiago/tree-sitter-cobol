module.exports = {
  //  ╭──────────────────────────────────────────────────────────╮
  //  │                          BLOCO                           │
  //  ╰──────────────────────────────────────────────────────────╯
  read_block: ($) =>
    prec.right(
      seq(
        seq($._READ, field("file", $.file_name)),
        opseq($._INTO, field("record", $.variable)),
        choice(
          seq($._AT, $._END, $.statement_block),
          seq($._NOT, $._AT, $._END, $.statement_block),
        ),

        $._END_READ,
      ),
    ),

  //  ╭──────────────────────────────────────────────────────────╮
  //  │                         SENTENÇA                         │
  //  ╰──────────────────────────────────────────────────────────╯
  read_sentence: ($) =>
    prec.right(
      seq(
        seq($._READ, field("file", $.file_name)),
        opseq($._INTO, field("record", $.variable)),
        choice(
          seq($._AT, $._END, $.atomic_imperative_block),
          seq($._NOT, $._AT, $._END, $.atomic_imperative_block),
        ),
      ),
    ),
};
