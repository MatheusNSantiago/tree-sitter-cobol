module.exports = {
  add_statement: ($) =>
    seq(kw("ADD"), choice($.variable, $.number), kw("TO"), repeat1($.variable)),

  subtract_statement: ($) =>
    seq(kw("SUBTRACT"), choice($.variable, $.number), kw("FROM"), $.variable),

  multiply_statement: ($) =>
    seq(kw("MULTIPLY"), choice($.variable, $.number), kw("BY"), $.variable),

  // ╭──────────────────────────────────────────────────────────╮
  // │                         COMPUTE                          │
  // ╰──────────────────────────────────────────────────────────╯
  compute_statement: ($) =>
    choice(
      $._compute_inline, //
      // $._compute_block,
    ),

  _compute_inline: ($) =>
    seq(
      $._COMPUTE,
      field("left", $.variable),
      choice("=", $._EQUAL),
      field("right", $.expr),
    ),

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
