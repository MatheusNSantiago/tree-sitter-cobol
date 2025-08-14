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
    prec.right(
      seq(
        $._COMPUTE,
        // choice(
        //   prec(1, $.compute_inline),
        //   prec(0, $.compute_block),
        // ),
        field("left", $.variable),
        choice("=", $._EQUAL),
        field("right", $.expr),
        op($.on_size_error), //
        op(kw("END-COMPUTE")),
      ),
    ),

  // compute_inline: ($) =>
  //   seq(
  //     field("left", $.variable),
  //     choice("=", $._EQUAL),
  //     field("right", $.expr),
  //   ),
  //
  // compute_block: ($) =>
  //   seq(
  //     $.expr,
  //     op($.on_size_error), //
  //     kw("END-COMPUTE"),
  //   ),

  on_size_error: ($) =>
    prec.right(
      seq(
        kw("ON"),
        kw("SIZE"),
        kw("ERROR"),
        repeat1($._statement),
      ),
    ),
};
