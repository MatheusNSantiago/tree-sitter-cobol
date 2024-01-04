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
      C($),
      repeat($._statement), //
    ),

  section_header: ($) => seq($.section_name, $._SECTION, "."),
};
