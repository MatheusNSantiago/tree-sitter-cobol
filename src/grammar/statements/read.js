module.exports = {
  read_statement: ($) =>
    prec.right(
      seq(
        kw("READ"),
        field("file", $.file_name),
        $._INTO,
        field("record", $.variable),
        op(seq(C($), repeat(choice($.at_end, $.not_at_end)))),
        optional(kw("END-READ")),
      ),
    ),

  at_end: ($) => prec.right(seq($._AT, $._END, repeat1($._statement))),
  not_at_end: ($) =>
    prec.right(seq($._NOT, $._AT, $._END, repeat1($._statement))),
};
