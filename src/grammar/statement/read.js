module.exports = {
  read_statement: ($) =>
    prec.right(
      seq(
        kw("READ"),
        $.file_name,
        $._INTO,
        $.variable,
        optional(
          seq(
            C($),
            repeat(choice($.at_end, $.not_at_end, $._statement)),
            kw("END-READ"),
          ),
        ),
        op("."),
      ),
    ),

  at_end: ($) => seq($._AT, $._END),
  not_at_end: ($) => seq($._NOT, $._AT, $._END),
};
