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

  // ASSIGN [TO] consolidado
  _ASSIGN_TO: (_) => /[aA][sS][sS][iI][gG][nN]([ \t]+[tT][oO])?/,
  assign_clause: ($) => seq($._ASSIGN_TO, field("dd_name", $.WORD)),

  // ACCESS [MODE] [IS] consolidado
  _ACCESS_MODE_IS: (_) => /[aA][cC][cC][eE][sS][sS]([ \t]+[mM][oO][dD][eE])?([ \t]+[iI][sS])?/,
  access_mode_clause: ($) =>
    seq(
      $._ACCESS_MODE_IS,
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

  // RELATIVE [KEY] [IS] consolidado
  _RELATIVE_KEY_IS: (_) => /[rR][eE][lL][aA][tT][iI][vV][eE]([ \t]+[kK][eE][yY])?([ \t]+[iI][sS])?/,
  relative_key_clause: ($) =>
    seq($._RELATIVE_KEY_IS, field("relative_key", $.WORD)),

  // FILE STATUS [IS] consolidado
  _FILE_STATUS_IS: (_) => /[fF][iI][lL][eE][ \t]+[sS][tT][aA][tT][uU][sS]([ \t]+[iI][sS])?/,
  file_status_clause: ($) =>
    seq($._FILE_STATUS_IS, field("reference", $.WORD)),

  // RECORD [KEY] [IS] consolidado
  _RECORD_KEY_IS: (_) => /[rR][eE][cC][oO][rR][dD]([ \t]+[kK][eE][yY])?([ \t]+[iI][sS])?/,
  record_key_clause: ($) =>
    seq($._RECORD_KEY_IS, field("reference", $.WORD)),
};
