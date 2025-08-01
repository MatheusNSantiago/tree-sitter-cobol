module.exports = {
  add_statement: ($) =>
    seq(
      kw("ADD"),
      repeat1(field("right", choice($.variable, $.number))),
      choice($._TO, $._GIVING),
      repeat1(field("left", $.variable)),
    ),

  subtract_statement: ($) =>
    seq(
      kw("SUBTRACT"),
      repeat1(field("right", choice($.variable, $.number))),
      $._FROM,
      repeat1(field("left", $.variable)),
    ),

  multiply_statement: ($) =>
    seq(
      seq(kw("MULTIPLY"), choice($.variable, $.number)),
      seq($._BY, $.variable),
      field("giving", opseq($._GIVING, $.variable)), //
    ),

  divide_statement: ($) =>
    seq(
      seq(kw("DIVIDE"), choice($.variable, $.number)),
      seq($._INTO, $.variable),
      repeat(
        choice(
          field("giving", seq($._GIVING, $.variable)), //
          field("remainder", seq(kw("REMAINDER"), $.variable)),
        ),
      ),
    ),

  // ╭──────────────────────────────────────────────────────────╮
  // │                         COMPUTE                          │
  // ╰──────────────────────────────────────────────────────────╯
  compute_statement: ($) =>
    seq(
      $._COMPUTE,
      choice(
        $._compute_inline, //
        $._compute_block,
      ),
    ),

  _compute_inline: ($) =>
    seq(
      field("left", $.variable),
      choice("=", $._EQUAL),
      field("right", $.expr),
    ),

  _compute_block: ($) =>
    seq(
      $.expr,
      op($.on_size_error), //
      kw("END-COMPUTE"),
    ),

  on_size_error: ($) =>
    seq(
      kw("ON"),
      kw("SIZE"),
      kw("ERROR"),
      choice($._CONTINUE, repeat1($._statement)),
    ),
};
