module.exports = {
  expr: ($) =>
    prec.left(
      choice(
        seq($._NOT, $.expr),
        seq($.expr, choice($._AND, $._OR), $.expr),
        $._expr_bool,
        // paren($.expr),
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
        $.string,
        $.constant,
      ),
    ),

  _expr_calc: ($) =>
    choice(
      $._expr_calc_binary,
      $._expr_calc_unary,
      $._expr_data,
      paren($.expr),
      seq($._LENGTH, optional($._OF), $.variable),
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
        // repeat(
        //   seq(
        //     choice(
        //       $._AND_LT,
        //       $._AND_LE,
        //       $._AND_GT,
        //       $._AND_GE,
        //       $._AND_EQ,
        //       $._AND_NE,
        //       $._OR_LT,
        //       $._OR_LE,
        //       $._OR_GT,
        //       $._OR_GE,
        //       $._OR_EQ,
        //       $._OR_NE,
        //     ),
        //     $._expr_calc,
        //   ),
        // ),
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
    prec(
      3,
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

  eq: ($) => seq(optional($._IS), choice($._EQUAL, "="), optional($._TO)),

  ge: ($) =>
    choice(
      seq(optional($._IS), ">="),
      seq(optional($._IS), $._NOT, "<"),
      seq(
        optional($._IS),
        $._GREATER,
        optional($._THAN),
        optional($._OR),
        $._EQUAL,
        optional($._TO),
      ),
      seq(optional($._IS), $._NOT_LESS, optional($._THAN)),
    ),

  le: ($) =>
    choice(
      seq(optional($._IS), "<="),
      seq(optional($._IS), $._NOT, ">"),
      seq(
        optional($._IS),
        $._LESS,
        optional($._THAN),
        optional($._OR),
        $._EQUAL,
        optional($._TO),
      ),
      seq(optional($._IS), $._NOT_GREATER, optional($._THAN)),
    ),

  ne: ($) =>
    choice(
      seq(optional($._IS), $._NOT_EQUAL, optional($._TO)), //
      "<>",
    ),

  gt: ($) =>
    choice(
      seq(optional($._IS), ">"),
      seq(optional($._IS), $._NOT, "<="),
      seq(optional($._IS), $._GREATER, optional($._THAN)),
    ),

  lt: ($) =>
    choice(
      seq(optional($._IS), "<"),
      seq(optional($._IS), $._NOT, ">="),
      seq(optional($._IS), $._LESS, optional($._THAN)),
    ),

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

  _NOT_EQUAL: (_) => /(!=)|([nN][oO][tT][ \t]+(([eE][qQ][uU][aA][lL])|=))/,
  _NOT_LESS: (_) => /([nN][oO][tT][ \t]+(<|[lL][eE][sS][sS]))/,
  _NOT_GREATER: (_) => /([nN][oO][tT][ \t]+(>|[gG][rR][eE][aA][tT][eE][rR]))/,

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
