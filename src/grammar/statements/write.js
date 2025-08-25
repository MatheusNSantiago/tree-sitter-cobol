module.exports = {
  write_statement: ($) => choice($._write_block, $._write_sentence),

  _write_block: ($) =>
    seq(
      $._WRITE,
      field("record_name", $.variable),
      opseq($._FROM, field("from", $.variable)),
      $._END_WRITE,
    ),

  _write_sentence: ($) =>
    seq(
      $._WRITE,
      field("record_name", $.variable),
      opseq($._FROM, field("from", $.variable)),
    ),
};
