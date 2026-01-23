module.exports = {
  expr: ($) =>
    prec.left(
      choice(
        seq($._NOT, $.expr),
        seq($.expr, choice($._AND, $._OR), $.expr),
        $._expr_bool,
        paren($.expr),
      ),
    ),

  _expr_data: ($) =>
    prec.left(
      choice(
        $.boolean,
        $.number,
        $.variable,
        // $.tab_field,
        // seq(":", $.variable),
        $.function,
        $.string,
        $.constant,
      ),
    ),

  function: ($) =>
    prec.left(
      seq(
        $._FUNCTION,
        field("name", $.WORD),
        parenOrNot(sep1($._expr_calc, ",")),
      ),
    ),

  // LENGTH [OF] consolidado em regex
  _LENGTH_OF: (_) => /[lL][eE][nN][gG][tT][hH]([ \t]+[oO][fF])?/,

  _expr_calc: ($) =>
    prec(
      1,
      choice(
        $._expr_calc_binary,
        $._expr_calc_unary,
        $._expr_data,
        paren($._expr_calc),
        seq($._LENGTH_OF, $.variable),
      ),
    ),

  _expr_calc_binary: ($) =>
    choice(
      prec.left(1, seq($._expr_calc, "+", $._expr_calc)),
      prec.left(1, seq($._expr_calc, "-", $._expr_calc)),
      prec.left(2, seq($._expr_calc, "**", $._expr_calc)),
      prec.left(2, seq($._expr_calc, "*", $._expr_calc)),
      prec.left(2, seq($._expr_calc, "/", $._expr_calc)),
      prec.left(3, seq($._expr_calc, "^", $._expr_calc)),
    ),

  _expr_calc_unary: ($) =>
    prec(
      4,
      choice(
        seq("+", $._expr_calc),
        seq("-", $._expr_calc),
        seq("^", $._expr_calc),
      ),
    ),

  _expr_compare: ($) =>
    prec.left(
      -1,
      seq(
        $._expr_calc,
        $._comparator,
        $._expr_calc,
        repeat(
          seq(
            choice(
              $._AND_LT,
              $._AND_LE,
              $._AND_GT,
              $._AND_GE,
              $._AND_EQ,
              $._AND_NE,
              $._OR_LT,
              $._OR_LE,
              $._OR_GT,
              $._OR_GE,
              $._OR_EQ,
              $._OR_NE,
            ),
            $._expr_calc,
          ),
        ),
      ),
    ),

  _comparator: ($) => choice($.ne, $.eq, $.gt, $.lt, $.ge, $.le),

  _expr_is: ($) =>
    prec(
      1,
      choice(
        seq(
          $._expr_calc,
          optional($._IS),
          choice(
            // $._OMITTED,
            $._NUMERIC,
            $._ALPHABETIC,
            $._ALPHABETIC_LOWER,
            $._ALPHABETIC_UPPER,
            // $._CLASS_NAME,
            $._POSITIVE,
            $._NEGATIVE,
            $._ZERO,

            // $._NOT_OMITTED,
            $._NOT_NUMERIC,
            $._NOT_ALPHABETIC,
            $._NOT_ALPHABETIC_LOWER,
            $._NOT_ALPHABETIC_UPPER,
            // $._NOT_CLASS_NAME,
            $._NOT_POSITIVE,
            $._NOT_NEGATIVE,
            $._NOT_ZERO,
          ),
        ),
      ),
    ),

  _expr_bool: ($) =>
    prec.right(
      1,
      choice(
        $._expr_is,
        $._expr_compare,
        $._expr_calc,
        // $.is_class,
        // $.is_not_class,
      ),
    ),

  _exp: ($) =>
    choice(
      // $._arith_x,
      $._binary_exp,
      $._unary_exp,
      paren($._exp),
    ),

  _binary_exp: ($) =>
    choice($.pow_exp, $.mul_exp, $.div_exp, $.add_exp, $.sub_exp),

  _unary_exp: ($) => prec(4, choice($.positive_exp, $.negative_exp)),

  positive_exp: ($) => seq("+", field("value", $._exp)),

  negative_exp: ($) => seq("-", field("value", $._exp)),

  add_exp: ($) =>
    prec.left(1, seq(field("left", $._exp), "+", field("right", $._exp))),

  sub_exp: ($) =>
    prec.left(1, seq(field("left", $._exp), "-", field("right", $._exp))),

  mul_exp: ($) =>
    prec.left(2, seq(field("left", $._exp), "*", field("right", $._exp))),

  div_exp: ($) =>
    prec.left(2, seq(field("left", $._exp), "/", field("right", $._exp))),

  pow_exp: ($) =>
    prec.left(3, seq(field("left", $._exp), "^", field("right", $._exp))),

  //todo
  // _arith_x: ($) => $._expr_data,
  // _arith_x: ($) =>
  // choice(
  // $._identifier,
  // seq($._LENGTH, optional($._OF), $._identifier),
  // seq($._LENGTH, optional($._OF), $._basic_literal),
  // seq($._LENGTH, optional($._OF), $.function_),
  // $._basic_literal,
  // $.function_,
  // $.linage_counter,
  // ),

  // is_class: ($) => prec(1, seq(field("x", $._x), field("class", $.WORD))),

  // is_not_class: ($) =>
  // prec(1, seq(field("x", $._x), $._NOT, field("class", $.WORD))),

  // Comparadores simplificados usando regex para reduzir estados
  // Padrões COBOL: IS EQUAL TO, =, IS NOT EQUAL, etc.

  eq: ($) => choice($._EQ_SYMBOL, $._EQ_WORD),
  _EQ_SYMBOL: (_) => /([iI][sS][ \t]+)?=/,
  _EQ_WORD: (_) => /([iI][sS][ \t]+)?[eE][qQ][uU][aA][lL]([ \t]+[tT][oO])?/,

  ne: ($) => choice($._NE_SYMBOL, $._NE_WORD),
  _NE_SYMBOL: (_) => /([iI][sS][ \t]+)?(<>|!=)/,
  // NOT = , NOT EQUAL, NOT EQUAL TO
  _NE_WORD: (_) => /([iI][sS][ \t]+)?[nN][oO][tT][ \t]+(=|[eE][qQ][uU][aA][lL]([ \t]+[tT][oO])?)/,

  gt: ($) => choice($._GT_WORD, ">"),
  _GT_WORD: (_) => /([iI][sS][ \t]+)?[gG][rR][eE][aA][tT][eE][rR]([ \t]+[tT][hH][aA][nN])?/,

  lt: ($) => choice($._LT_WORD, "<"),
  _LT_WORD: (_) => /([iI][sS][ \t]+)?[lL][eE][sS][sS]([ \t]+[tT][hH][aA][nN])?/,

  ge: ($) => choice($._GE_SYMBOL, $._GE_WORD, $._GE_NOT_LESS),
  _GE_SYMBOL: (_) => /([iI][sS][ \t]+)?>=/,
  _GE_WORD: (_) => /([iI][sS][ \t]+)?[gG][rR][eE][aA][tT][eE][rR]([ \t]+[tT][hH][aA][nN])?([ \t]+[oO][rR][ \t]+[eE][qQ][uU][aA][lL]([ \t]+[tT][oO])?)?/,
  _GE_NOT_LESS: (_) => /([iI][sS][ \t]+)?[nN][oO][tT][ \t]+(<|[lL][eE][sS][sS]([ \t]+[tT][hH][aA][nN])?)/,

  le: ($) => choice($._LE_SYMBOL, $._LE_WORD, $._LE_NOT_GREATER),
  _LE_SYMBOL: (_) => /([iI][sS][ \t]+)?<=/,
  _LE_WORD: (_) => /([iI][sS][ \t]+)?[lL][eE][sS][sS]([ \t]+[tT][hH][aA][nN])?([ \t]+[oO][rR][ \t]+[eE][qQ][uU][aA][lL]([ \t]+[tT][oO])?)?/,
  _LE_NOT_GREATER: (_) => /([iI][sS][ \t]+)?[nN][oO][tT][ \t]+(>|[gG][rR][eE][aA][tT][eE][rR]([ \t]+[tT][hH][aA][nN])?)/,

  // ╭──────────────────────────────────────────────────────────╮
  // │                         keywords                         │
  // ╰──────────────────────────────────────────────────────────╯

  _AND_LT: (_) =>
    /[aA][nN][dD][ \t]+(<|[lL][eE][sS][sS][ \t]+[tT][hH][aA][nN])/,
  _AND_LE: (_) =>
    /[aA][nN][dD][ \t]+(<=|[nN][oO][tT][ \t]+(>|[gG][rR][eE][aA][tT][eE][rR][ \t]+[tT][hH][aA][nN]))/,
  _AND_GT: (_) =>
    /[aA][nN][dD][ \t]+(>|[gG][rR][eE][aA][tT][eE][rR][ \t]+[tT][hH][aA][nN])/,
  _AND_GE: (_) =>
    /[aA][nN][dD][ \t]+(>=|[nN][oO][tT][ \t]+(<|[lL][eE][sS][sS][ \t]+[tT][hH][aA][nN]))/,
  _AND_EQ: (_) => /[aA][nN][dD][ \t]+(=|[eE][qQ][uU][aA][lL]([ \t]+[tT][oO])?)/,
  _AND_NE: (_) =>
    /[aA][nN][dD][ \t]+(!=|[nN][oO][tT][ \t]+[eE][qQ][uU][aA][lL]([ \t]+[tT][oO])?)/,
  _OR_LT: (_) => /[oO][rR][ \t]+(<|[lL][eE][sS][sS][ \t]+[tT][hH][aA][nN])/,
  _OR_LE: (_) =>
    /[oO][rR][ \t]+(<=|[nN][oO][tT][ \t]+(>|[gG][rR][eE][aA][tT][eE][rR][ \t]+[tT][hH][aA][nN]))/,
  _OR_GT: (_) =>
    /[oO][rR][ \t]+(>|[gG][rR][eE][aA][tT][eE][rR][ \t]+[tT][hH][aA][nN])/,
  _OR_GE: (_) =>
    /[oO][rR][ \t]+(>=|[nN][oO][tT][ \t]+(<|[lL][eE][sS][sS][ \t]+[tT][hH][aA][nN]))/,
  _OR_EQ: (_) => /[oO][rR][ \t]+(=|[eE][qQ][uU][aA][lL]([ \t]+[tT][oO])?)/,
  _OR_NE: (_) =>
    /[oO][rR][ \t]+(!=|[nN][oO][tT][ \t]+[eE][qQ][uU][aA][lL]([ \t]+[tT][oO])?)/,

  // _NOT_EQUAL e _NOT_GREATER movidos para regex nos comparadores

  _NUMERIC: (_) => kw("NUMERIC"),
  _NOT_NUMERIC: ($) => seq($._NOT, $._NUMERIC),

  _ALPHABETIC: (_) => kw("ALPHABETIC"),
  _NOT_ALPHABETIC: ($) => seq($._NOT, $._ALPHABETIC),

  _ALPHABETIC_LOWER: (_) => kw("ALPHABETIC-LOWER"),
  _NOT_ALPHABETIC_LOWER: ($) => seq($._NOT, $._ALPHABETIC_LOWER),

  _ALPHABETIC_UPPER: (_) => kw("ALPHABETIC-UPPER"),
  _NOT_ALPHABETIC_UPPER: ($) => seq($._NOT, $._ALPHABETIC_UPPER),

  _POSITIVE: (_) => kw("POSITIVE"),
  _NOT_POSITIVE: ($) => seq($._NOT, $._POSITIVE),

  _NEGATIVE: (_) => kw("NEGATIVE"),
  _NOT_NEGATIVE: ($) => seq($._NOT, $._NEGATIVE),

  _ZERO: (_) => kw("ZERO"),
  _NOT_ZERO: ($) => seq($._NOT, $._ZERO),
};
