module.exports = {
  environment_division: ($) =>
    seq(
      seq(field("division_header", $.environment_division_header), "."),
      repeat(
        choice(
          $.configuration_section, //
          $.input_output_section,
        ),
      ),
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
  configuration_section_header: ($) => seq($._CONFIGURATION, $._SECTION),

  _configuration_paragraph: ($) => seq(choice($.special_names_paragraph), C($)),

  special_names_paragraph: ($) =>
    prec.right(
      seq(
        seq(field("paragraph_header", $.special_names_paragraph_header), "."),
        // optional(seq($.special_name, ".")),
        repeat(seq($.special_name, optional("."))),
        C($),
      ),
    ),
  special_names_paragraph_header: (_) => kw("SPECIAL-NAMES"),

  special_name: ($) => choice($.decimal_point_clause, $.mnemonic_name_clause),

  decimal_point_clause: ($) =>
    seq(kw("DECIMAL-POINT"), optional($._IS), kw("COMMA")),

  mnemonic_name_clause: ($) =>
    seq(field("word", $.WORD), optional($._IS), field("value", $.WORD)),

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
      repeat(
        seq(
          seq($.select_statement, "."), //
          C($),
        ),
      ),
    ),
  file_control_paragraph_header: ($) => $._FILE_CONTROL,

  //  ╭──────────────────────────────────────────────────────────╮
  //  │                  SELECT FILE STATEMENT                   │
  //  ╰──────────────────────────────────────────────────────────╯

  select_statement: ($) =>
    seq($._SELECT, field("file", $.file_name), repeat($._select_clause)),

  _select_clause: ($) =>
    choice(
      $.assign_clause,
      $.access_mode_clause,
      $.organization_clause,
      $.relative_key_clause,
      $.file_status_clause,
      $.record_key_clause,
    ),

  assign_clause: ($) => seq($._ASSIGN, op($._TO), field("dd_name", $.WORD)),

  access_mode_clause: ($) =>
    seq(
      kw("ACCESS"),
      op($._MODE),
      op($._IS),
      choice($._SEQUENTIAL, kw("DYNAMIC"), kw("RANDOM")),
    ),

  organization_clause: ($) =>
    seq(
      optional(seq(kw("ORGANIZATION"), optional($._IS))),
      choice(
        kw("INDEXED"),
        seq($._RECORD, optional(kw("BINARY")), $._SEQUENTIAL),
        $._SEQUENTIAL,
        $._RELATIVE,
      ),
    ),

  relative_key_clause: ($) =>
    seq($._RELATIVE, op($._KEY), op($._IS), field("relative_key", $.WORD)),

  file_status_clause: ($) =>
    seq(
      kw("FILE"),
      kw("STATUS"),
      optional($._IS), //
      field("reference", $.WORD),
    ),

  record_key_clause: ($) =>
    seq(
      $._RECORD,
      optional($._KEY),
      optional($._IS),
      field("reference", $.WORD),
    ),
};
