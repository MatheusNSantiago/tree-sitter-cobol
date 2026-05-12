module.exports = {
  call_statement: ($) =>
    seq(
      $._CALL,
      field("program", choice($.variable, $.string)),
      field(
        "using",
        opseq(
          $._USING,
          seq($.variable, repeat(seq(op(","), $.variable))), //
        ),
      ),
    ),
};
