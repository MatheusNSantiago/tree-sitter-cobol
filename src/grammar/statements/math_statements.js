module.exports = {
  _data_reference_list: ($) => repeat1(choice($.variable, $.number)),
  _variable_list: ($) => repeat1($.variable),

  //  ╭──────────────────────────────────────────────────────────╮
  //  │                        SELETORES                         │
  //  ╰──────────────────────────────────────────────────────────╯

  // add_statement: ($) => choice(prec(1, $.add_block), $.add_sentence),
  //
  // subtract_statement: ($) =>
  //   choice(prec(1, $.subtract_block), $.subtract_sentence),
  //
  // multiply_statement: ($) =>
  //   choice(prec(1, $.multiply_block), $.multiply_sentence),
  //
  // divide_statement: ($) => choice(prec(1, $.divide_block), $.divide_sentence),
  //
  // compute_statement: ($) =>
  //   choice(prec(1, $.compute_block), $.compute_sentence),

  //  ╭──────────────────────────────────────────────────────────╮
  //  │                           ADD                            │
  //  ╰──────────────────────────────────────────────────────────╯
  add_block: ($) =>
    prec.right(
      seq(
        op($._CORRESPONDING),
        seq($._ADD, field("amount", $._data_reference_list)),
        choice(
          seq($._TO, field("to", $._variable_list)),
          seq($._GIVING, field("result", $._variable_list)),
        ),
        op($._on_size_error_block),
        $._END_ADD,
      ),
    ),

  add_sentence: ($) =>
    prec.right(
      seq(
        op($._CORRESPONDING),
        seq($._ADD, field("amount", $._data_reference_list)),
        choice(
          seq($._TO, field("to", $._variable_list)),
          seq($._GIVING, field("result", $._variable_list)),
        ),
        op($._on_size_error_sentence),
      ),
    ),

  //  ╭──────────────────────────────────────────────────────────╮
  //  │                         SUBTRACT                         │
  //  ╰──────────────────────────────────────────────────────────╯

  subtract_block: ($) =>
    prec.right(
      seq(
        seq($._SUBTRACT, field("left", $._data_reference_list)),
        seq($._FROM, field("right", $._variable_list)),
        opseq($._GIVING, field("result", $._variable_list)),
        op($._on_size_error_block),
        $._END_SUBTRACT,
      ),
    ),

  subtract_sentence: ($) =>
    prec.right(
      seq(
        seq($._SUBTRACT, field("left", $._data_reference_list)),
        seq($._FROM, field("right", $._variable_list)),
        opseq($._GIVING, field("result", $._variable_list)),
        op($._on_size_error_sentence),
      ),
    ),

  //  ╭──────────────────────────────────────────────────────────╮
  //  │                         MULTIPLY                         │
  //  ╰──────────────────────────────────────────────────────────╯

  multiply_block: ($) =>
    prec.right(
      seq(
        seq($._MULTIPLY, field("left", choice($.variable, $.number))),
        seq($._BY, field("right", $._data_reference_list)),
        opseq($._GIVING, field("result", $._variable_list)),
        op($._on_size_error_block),
        $._END_MULTIPLY,
      ),
    ),

  multiply_sentence: ($) =>
    prec.right(
      seq(
        seq($._MULTIPLY, field("left", choice($.variable, $.number))),
        seq($._BY, field("right", $._data_reference_list)),
        opseq($._GIVING, field("result", $._variable_list)),
        op($._on_size_error_sentence),
      ),
    ),

  //  ╭──────────────────────────────────────────────────────────╮
  //  │                          DIVIDE                          │
  //  ╰──────────────────────────────────────────────────────────╯

  divide_block: ($) =>
    prec.right(
      seq(
        seq($._DIVIDE, field("left", choice($.variable, $.number))),
        choice(
          seq($._BY, field("foo", $._data_reference_list)),
          seq($._INTO, field("result", $._variable_list)),
        ),
        opseq($._GIVING, field("result", $._variable_list)),
        opseq($._REMAINDER, field("remainder", $.variable)),
        op($._on_size_error_block),
        $._END_DIVIDE,
      ),
    ),

  divide_sentence: ($) =>
    prec.right(
      seq(
        seq($._DIVIDE, field("left", choice($.variable, $.number))),
        choice(
          seq($._INTO, field("result", $._variable_list)),
          seq($._BY, field("right", $._data_reference_list)),
        ),
        opseq($._GIVING, field("result", $._variable_list)),
        opseq($._REMAINDER, field("remainder", $.variable)),
        op($._on_size_error_sentence),
      ),
    ),

  // ╭──────────────────────────────────────────────────────────╮
  // │                         COMPUTE                          │
  // ╰──────────────────────────────────────────────────────────╯

  compute_block: ($) =>
    prec.right(
      seq(
        seq($._COMPUTE, repeat1(field("left", $.variable))),
        choice("=", $._EQUAL),
        field("right", $.expr),
        op($._on_size_error_block),
        $._END_COMPUTE,
      ),
    ),

  compute_sentence: ($) =>
    prec.right(
      seq(
        seq($._COMPUTE, repeat1(field("left", $.variable))),
        choice("=", $._EQUAL),
        field("right", $.expr),
        op($._on_size_error_sentence),
      ),
    ),

  _on_size_error_block: ($) =>
    seq(
      seq($._ON, op($._SIZE), $._ERROR),
      $.statement_block, // dentro de bloco pode ter qualquer tipo de statement
    ),

  // https://bs2manuals.ts.fujitsu.com/psCOBOL2000V16en/cobol2000-reference-manual-21133/procedure-division-cobol-compiler-reference-manual-2022-11-177/phrases-in-statements-cobol-compiler-reference-manual-2022-11-193/on-size-error-phrase-cobol-compiler-reference-manual-2022-11-197
  // On size error
  _on_size_error_sentence: ($) =>
    seq(seq($._ON, op($._SIZE), $._ERROR), $.atomic_imperative_block),
};
