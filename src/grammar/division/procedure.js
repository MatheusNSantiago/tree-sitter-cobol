module.exports = {
  procedure_division: ($) =>
    seq(
      field("division_header", $.procedure_division_header),
      op($.procedure_using),
      ".",
      C($),
      repeat($.paragraph),
      repeat($.section),
    ),

  procedure_division_header: ($) => seq(kw("PROCEDURE"), $._DIVISION),

  procedure_using: ($) => seq(kw("USING"), repeat1($.variable)),

  // ╭──────────────────────────────────────────────────────────╮
  // │                        Sections                          │
  // ╰──────────────────────────────────────────────────────────╯

  section: ($) =>
    seq(
      field("section_header", $.section_header),
      repeat(choice($._statement, $.paragraph)),
    ),

  // ╭──────────────────────────────────────────────────────────╮
  // │                        Paragraphs                        │
  // ╰──────────────────────────────────────────────────────────╯

  paragraph: ($) =>
    prec.right(
      seq(
        field("paragraph_header", $.paragraph_header), //
        repeat($._statement),
      ),
    ),
};
