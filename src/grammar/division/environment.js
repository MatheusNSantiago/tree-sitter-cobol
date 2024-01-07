module.exports = {
  environment_division: ($) =>
    seq(
      seq(field("division_header", $.environment_division_header), "."),
      optional($.configuration_section),
      optional($.input_output_section),
    ),

  environment_division_header: ($) => seq(kw("ENVIRONMENT"), $._DIVISION),

  // ╭──────────────────────────────────────────────────────────╮
  // │                  CONFIGURATION SECTION                   │
  // ╰──────────────────────────────────────────────────────────╯

  configuration_section: ($) =>
    seq(
      seq(field("section_header", $.configuration_section_header), "."),
      repeat($._configuration_paragraph),
    ),
  configuration_section_header: ($) => seq(kw("CONFIGURATION"), $._SECTION),

  _configuration_paragraph: ($) => seq(choice($.special_names_paragraph), C($)),

  special_names_paragraph: ($) =>
    prec.right(
      seq(
        seq(field("paragraph_header", $.special_names_paragraph_header), "."),
        optional(seq($.special_name, ".")),
        C($),
      ),
    ),
  special_names_paragraph_header: (_) => kw("SPECIAL-NAMES"),

  special_name: ($) => seq(choice($.decimal_point_clause)),

  decimal_point_clause: ($) =>
    seq(kw("DECIMAL-POINT"), optional($._IS), kw("COMMA")),

  // ╭──────────────────────────────────────────────────────────╮
  // │                   INPUT-OUTPUT SECTION                   │
  // ╰──────────────────────────────────────────────────────────╯

  input_output_section: ($) =>
    seq(
      seq(field("section_header", $.input_output_section_header), "."),
      optional($.file_control_paragraph),
    ),
  input_output_section_header: ($) => seq(kw("INPUT-OUTPUT"), $._SECTION),

  file_control_paragraph: ($) =>
    seq(
      seq(field("paragraph_header", $.file_control_paragraph_header), "."),
      C($),
      repeat(seq($.select_statement, C($))),
    ),
  file_control_paragraph_header: (_) => kw("FILE-CONTROL"),
};
