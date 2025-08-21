module.exports = {
  _data_reference_list: ($) => repeat1(choice($.variable, $.number)),
  _variable_list: ($) => repeat1($.variable),

  //  ╭──────────────────────────────────────────────────────────╮
  //  │                        SELETORES                         │
  //  ╰──────────────────────────────────────────────────────────╯

  add_statement: ($) => choice($._add_block, $._add_sentence),
  subtract_statement: ($) => choice($._subtract_block, $._subtract_sentence),
  multiply_statement: ($) => choice($._multiply_block, $._multiply_sentence),
  divide_statement: ($) => choice($._divide_block, $._divide_sentence),
  compute_statement: ($) => choice($._compute_block, $._compute_sentence),

  //  ╭──────────────────────────────────────────────────────────╮
  //  │                           ADD                            │
  //  ╰──────────────────────────────────────────────────────────╯
  _add_block: ($) =>
    prec.right(
      seq(
        op(choice($._CORRESPONDING, $._CORR)),
        seq($._ADD, field("amount", $._data_reference_list)),
        seq($._TO, field("to", $._variable_list)),
        opseq($._GIVING, field("result", $._variable_list)),
        op($._on_size_error_block),
        $._END_ADD,
      ),
    ),

  _add_sentence: ($) =>
    prec.right(
      seq(
        op(choice($._CORRESPONDING, $._CORR)),
        seq($._ADD, field("amount", $._data_reference_list)),
        seq($._TO, field("to", $._variable_list)),
        opseq($._GIVING, field("result", $._variable_list)),
        op($._on_size_error_sentence),
      ),
    ),

  //  ╭──────────────────────────────────────────────────────────╮
  //  │                         SUBTRACT                         │
  //  ╰──────────────────────────────────────────────────────────╯

  _subtract_block: ($) =>
    prec.right(
      seq(
        seq($._SUBTRACT, field("left", $._data_reference_list)),
        seq($._FROM, field("right", $._variable_list)),
        opseq($._GIVING, field("result", $._variable_list)),
        op($._on_size_error_block),
        $._END_SUBTRACT,
      ),
    ),

  _subtract_sentence: ($) =>
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

  _multiply_block: ($) =>
    prec.right(
      seq(
        seq($._MULTIPLY, field("left", choice($.variable, $.number))),
        seq($._BY, field("right", $._data_reference_list)),
        opseq($._GIVING, field("result", $._variable_list)),
        op($._on_size_error_block),
        $._END_MULTIPLY,
      ),
    ),

  _multiply_sentence: ($) =>
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

  _divide_block: ($) =>
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

  _divide_sentence: ($) =>
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

  _compute_block: ($) =>
    prec.right(
      seq(
        seq($._COMPUTE, repeat1(field("left", $.variable))),
        choice("=", $._EQUAL),
        field("right", $.expr),
        op($._on_size_error_block),
        $._END_COMPUTE,
      ),
    ),

  _compute_sentence: ($) =>
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

// module.exports = {
//   _data_reference_list: ($) => repeat1(choice($.variable, $.number)),
//   _variable_list: ($) => repeat1($.variable),
//
//   //  ╭──────────────────────────────────────────────────────────╮
//   //  │                           ADD                            │
//   //  ╰──────────────────────────────────────────────────────────╯
//   add_statement: ($) =>
//     prec.left(
//       seq(
//         op(choice($._CORRESPONDING, $._CORR)),
//         seq($._ADD, field("amount", $._data_reference_list)),
//         seq($._TO, field("to", $._variable_list)),
//         opseq($._GIVING, field("result", $._variable_list)),
//         choice(
//           seq(op($._on_size_error_block), $._END_ADD), // Bloco
//           $._on_size_error_sentence, // Sentença
//         ),
//       ),
//     ),
//
//   //  ╭──────────────────────────────────────────────────────────╮
//   //  │                         SUBTRACT                         │
//   //  ╰──────────────────────────────────────────────────────────╯
//
//   subtract_statement: ($) =>
//     seq(
//       seq($._SUBTRACT, field("left", $._data_reference_list)),
//       seq($._FROM, field("right", $._variable_list)),
//       opseq($._GIVING, field("result", $._variable_list)),
//       choice(
//         seq(op($._on_size_error_block), $._END_SUBTRACT), // Bloco
//         $._on_size_error_sentence, // Sentença
//       ),
//     ),
//
//   //  ╭──────────────────────────────────────────────────────────╮
//   //  │                         MULTIPLY                         │
//   //  ╰──────────────────────────────────────────────────────────╯
//
//   multiply_statement: ($) =>
//     seq(
//       seq($._MULTIPLY, field("left", choice($.variable, $.number))),
//       seq($._BY, field("right", $._data_reference_list)),
//       opseq($._GIVING, field("result", $._variable_list)),
//       choice(
//         seq(op($._on_size_error_block), $._END_MULTIPLY), // Bloco
//         $._on_size_error_sentence, // Sentença
//       ),
//     ),
//
//   //  ╭──────────────────────────────────────────────────────────╮
//   //  │                          DIVIDE                          │
//   //  ╰──────────────────────────────────────────────────────────╯
//
//   divide_statement: ($) =>
//     seq(
//       seq($._DIVIDE, field("left", choice($.variable, $.number))),
//       choice(
//         seq($._BY, field("by", $._data_reference_list)),
//         seq($._INTO, field("result", $._variable_list)),
//       ),
//       opseq($._GIVING, field("result", $._variable_list)),
//       opseq($._REMAINDER, field("remainder", $.variable)),
//       choice(
//         seq(op($._on_size_error_block), $._END_DIVIDE), // Bloco
//         $._on_size_error_sentence, // Sentença
//       ),
//     ),
//
//   // ╭──────────────────────────────────────────────────────────╮
//   // │                         COMPUTE                          │
//   // ╰──────────────────────────────────────────────────────────╯
//
//   compute_statement: ($) =>
//     seq(
//       seq($._COMPUTE, repeat1(field("left", $.variable))),
//       choice("=", $._EQUAL),
//       field("right", $.expr),
//       choice(
//         seq(op($._on_size_error_block), $._END_COMPUTE), // Bloco
//         $._on_size_error_sentence, // Sentença
//       ),
//     ),
//
//   _on_size_error_block: ($) =>
//     seq(
//       seq($._ON, op($._SIZE), $._ERROR),
//       $.statement_block, // dentro de bloco pode ter qualquer tipo de statement
//     ),
//
//   // https://bs2manuals.ts.fujitsu.com/psCOBOL2000V16en/cobol2000-reference-manual-21133/procedure-division-cobol-compiler-reference-manual-2022-11-177/phrases-in-statements-cobol-compiler-reference-manual-2022-11-193/on-size-error-phrase-cobol-compiler-reference-manual-2022-11-197
//   // On size error
//   _on_size_error_sentence: ($) =>
//     seq(seq($._ON, op($._SIZE), $._ERROR), $.atomic_imperative_block),
// };
