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
    ),

};
