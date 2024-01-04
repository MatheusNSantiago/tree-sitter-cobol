module.exports = {
  _WHEN: (_) => kw("WHEN"),

  evaluate_statement: ($) =>
      seq(
        seq(kw("EVALUATE"), $.expr), // EVALUEATE <expr>
        C($),
        repeat1(
          seq(
            seq($._WHEN, $.expr), // WHEN <expr>
            repeat($._statement), // <statement> ...
          ),
        ),
        optional(
          seq(
            seq($._WHEN, kw("OTHER")), // WHEN OTHER
            repeat($._statement), // <statement> ...
          ),
        ),
        kw("END-EVALUATE"), // END-EVALUATE
        optional("."),
    ),

  // evaluate_header: ($) =>
  //   prec.right(
  //     seq(kw("EVALUATE"), sepBy($.evaluate_subject, optional($._ALSO))),
  //   ),
  //
  // evaluate_subject: ($) => choice($.expr, $._TRUE, $._FALSE),
  //
  // when: ($) => prec.right(repeat1(seq($._WHEN, $._evaluate_object_list))),
  //
  // _evaluate_object_list: ($) =>
  //   prec.right(sepBy($._evaluate_object, optional($._ALSO))),
  //
  // _evaluate_object: ($) => choice($.expr, $._ANY, $._TRUE, $._FALSE),
  //
  // when_other: ($) => seq($._WHEN, $._OTHER),
};
