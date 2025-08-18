module.exports = {
  // =========================================================================
  // NÍVEL 1: COMANDOS ATÔMICOS
  // Verbos simples que não contêm outros statements.
  // =========================================================================
  _atomic_imperative: ($) =>
    choice(
      $.move_statement,
      $.display_statement,
      $.goback_statement,
      $.continue_statement,
      $.next_sentence_statement,
      $.copy_statement,
      $.exit_statement,
      $.stop_run_statement,
      $.call_statement,
      $.open_statement,
      $.close_statement,
      $.goto_statement,
      $.write_statement,
      $.set_statement,
      $.cancel_statement,
      $.exec_cics,
      $.exec_sql,
      $.string_statement,
      $.initialize_statement,
      $.sort_statement,
    ),

  // =========================================================================
  // NÍVEL 2: COMANDOS COMPOSTOS (as "sentenças" ambíguas)
  // Seus corpos SÓ podem conter Comandos Atômicos.
  // =========================================================================
  _compound_imperative: ($) =>
    choice(
      $.if_sentence,
      $.perform_sentence,
      $.read_sentence,
      $.search_sentence,
      $.add_sentence,
      $.subtract_sentence,
      $.multiply_sentence,
      $.divide_sentence,
      $.compute_sentence,
    ),

  // =========================================================================
  // NÍVEL 3: BLOCOS COM ESCOPO
  // Seus corpos podem conter qualquer coisa, pois são seguros.
  // =========================================================================
  _scoped_block: ($) =>
    choice(
      $.if_block,
      $.perform_block,
      $.evaluate_statement,
      $.read_block,
      $.search_block,
      $.add_block,
      $.subtract_block,
      $.multiply_block,
      $.divide_block,
      $.compute_block,
    ),
  // =========================================================================
  // A LISTA MESTRA ("_statement")
  // Um statement é um Bloco, um Composto ou um Atômico.
  // =========================================================================
  _statement: ($) =>
    choice(
      prec(1, $._atomic_imperative),
      prec(2, $._scoped_block),
      $._compound_imperative,
    ),

  // =========================================================================
  // REGRAS DE CORPO DE BLOCO
  // =========================================================================
  statement_block: ($) => repeat1($._statement),
  atomic_imperative_block: ($) => prec.left(repeat1($._atomic_imperative)),

  // =========================================================================
  // EXPORTS
  // =========================================================================
  ...require("./perform"),
  ...require("./if"),
  ...require("./evaluate"),
  ...require("./call"),
  ...require("./read"),
  ...require("./write"),
  ...require("./search"),
  ...require("./math_statements"),
  ...require("./exec_sql"),
  ...require("./exec_cics"),
  ...require("./move"),
  ...require("./sort"),

  exit_statement: ($) => $._EXIT,
  goback_statement: ($) => $._GOBACK,
  stop_run_statement: ($) => seq($._STOP, $._RUN),
  close_statement: ($) => seq($._CLOSE, repeat1($.file_name)),
  copy_statement: ($) => seq($._COPY, field("copybook", $.variable)),
  goto_statement: ($) => seq($._GO, optional($._TO), field("to", $.WORD)),
  set_statement: ($) =>
    seq(
      $._SET,
      $.variable,
      choice($._TO, seq(choice($._UP, $._DOWN), $._BY)),
      seq($._expr_data),
    ),

  cancel_statement: ($) => seq($._CANCEL, $.variable),
  next_sentence_statement: ($) => seq($._NEXT, $._SENTENCE),

  string_statement: ($) =>
    seq(
      seq($._STRING, field("from", repeat1($.string_item))),
      seq($._INTO, field("into", $.variable)),
      op($._END_STRING),
    ),

  string_item: ($) =>
    seq(choice($.string, $.variable), opseq($._DELIMITED, op($._BY), $._SIZE)),

  display_statement: ($) => seq($._DISPLAY, repeat1($._expr_data)),

  open_statement: ($) =>
    seq(
      $._OPEN,
      repeat1(
        choice(
          field("input", seq($._INPUT, repeat1($.file_name))),
          field("output", seq(kw("OUTPUT"), repeat1($.file_name))),
        ),
      ),
    ),

  initialize_statement: ($) =>
    seq(
      $._INITIALIZE,
      field("variable", repeat1($.variable)),
      field(
        "replacing",
        opseq($._REPLACING, repeat1($.initialize_replacing_item)),
      ),
    ),

  initialize_replacing_item: ($) =>
    seq(
      field("category", $._initialize_category),
      seq($._BY, field("by", $._value)),
    ),

  _initialize_category: ($) =>
    choice(
      $._ALPHABETIC,
      kw("ALPHANUMERIC"),
      kw("NUMERIC"),
      kw("ALPHANUMERIC_EDITED"),
      kw("NUMERIC_EDITED"),
      kw("NATIONAL"),
      kw("NATIONAL_EDITED"),
    ),

  continue_statement: ($) => $._CONTINUE,
};
