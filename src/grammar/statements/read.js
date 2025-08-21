module.exports = {
  read_statement: ($) => choice($._read_block, $._read_sentence),

  //  ╭──────────────────────────────────────────────────────────╮
  //  │                          BLOCO                           │
  //  ╰──────────────────────────────────────────────────────────╯
  _read_block: ($) =>
    seq(
      seq($._READ, field("file", $.file_name)),
      opseq($._INTO, field("record", $.variable)),
      choice(
        seq($._AT, $._END, $.statement_block),
        seq($._NOT, $._AT, $._END, $.statement_block),
      ),

      $._END_READ,
    ),

  //  ╭──────────────────────────────────────────────────────────╮
  //  │                         SENTENÇA                         │
  //  ╰──────────────────────────────────────────────────────────╯
  _read_sentence: ($) =>
    seq(
      seq($._READ, field("file", $.file_name)),
      opseq($._INTO, field("record", $.variable)),
      choice(
        seq($._AT, $._END, $.atomic_imperative_block),
        seq($._NOT, $._AT, $._END, $.atomic_imperative_block),
      ),
    ),
};
