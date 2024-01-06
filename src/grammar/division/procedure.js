module.exports = {
  procedure_division: ($) =>
    seq(
      field("division_header", $.procedure_division_header),
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

  section_header: ($) => seq($.section_name, kw("SECTION"), "."),

  // ╭──────────────────────────────────────────────────────────╮
  // │                        Paragraphs                        │
  // ╰──────────────────────────────────────────────────────────╯

  paragraph: ($) =>
    prec.right(
      seq(
        field("paragraph_header", $.PARAGRAPH_HEADER), //
        repeat($._statement),
      ),
    ),
};
