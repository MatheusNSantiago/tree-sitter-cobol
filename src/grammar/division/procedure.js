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
    prec.right(
      seq(
        field("section_header", $.section_header),
        C($),
        repeat($._statement),
        repeat($.paragraph),
      ),
    ),

  section_header: ($) => seq($.section_name, $._SECTION, "."),

  // ╭──────────────────────────────────────────────────────────╮
  // │                        Paragraphs                        │
  // ╰──────────────────────────────────────────────────────────╯

  paragraph: ($) =>
    seq(
      field("paragraph_header", $.paragraph_header),
      C($),
      repeat($._statement),
    ),

  paragraph_header: ($) => seq($.paragraph_name, "."),
};
