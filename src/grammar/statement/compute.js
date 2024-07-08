module.exports = {
  compute_statement: ($) => choice($._compute_inline, $._compute_block),

  _compute_inline: ($) => seq($._COMPUTE, $.expr),

  _compute_block: ($) =>
    seq(
      $._COMPUTE,
      $.expr,
      op($.on_size_error), //
      kw("END-COMPUTE"),
    ),

  on_size_error: ($) =>
    seq(
      kw("ON"),
      kw("SIZE"),
      kw("ERROR"),
      choice(kw("CONTINUE", repeat1($._statement))),
    ),

  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  _COMPUTE: (_) => kw("COMPUTE"),
};
