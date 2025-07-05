module.exports = {
  _WHEN: (_) => kw("WHEN"),

  evaluate_statement: ($) =>
    seq(
      seq(kw("EVALUATE"), $.expr), // EVALUEATE <expr>
      C($),
      repeat1(
        field(
          "case",
          prec.left(
            20,
            seq(
              seq($._WHEN, $.expr), // WHEN <expr>
              C($),
              repeat(
                choice(
                  $._statement, // <statement> ...
                  kw("CONTINUE"), // <continue> ...
                ),
              ),
            ),
          ),
        ),
      ),
      // optional(
      //   seq(
      //     seq($._WHEN, kw("OTHER")), // WHEN OTHER
      //     repeat($._statement), // <statement> ...
      //   ),
      // ),
      kw("END-EVALUATE"), // END-EVALUATE
    ),
};
