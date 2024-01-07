module.exports = {
  procedure_division: ($) =>
    seq(
      field("division_header", $.procedure_division_header),
      C($),
      repeat($.section),
    ),

  procedure_division_header: ($) => seq($._PROCEDURE, $._DIVISION, "."),

  // ╭──────────────────────────────────────────────────────────╮
  // │                        Sections                          │
  // ╰──────────────────────────────────────────────────────────╯

  section: ($) =>
    seq(
      field("section_header", $.section_header),
      repeat(choice($._statement, $.paragraph)),
    ),

  // section_header: ($) => seq($._SECTION_NAME, kw("SECTION"), "."),

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
