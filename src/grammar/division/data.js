module.exports = {
  data_division: ($) =>
    seq(
      seq(field("division_header", $.data_division_header), "."),
      optional($.file_section),
      optional($.working_storage_section),
    ),

  data_division_header: ($) => seq(kw("DATA"), $._DIVISION),

  // ╭──────────────────────────────────────────────────────────╮
  // │                       FILE SECTION                       │
  // ╰──────────────────────────────────────────────────────────╯

  file_section: ($) =>
    seq(
      seq(field("section_header", $.file_section_header), "."),
      repeat(
        seq(choice($.file_description, $.record_description), "."), //
      ),
    ),
  file_section_header: ($) => seq(kw("FILE"), $._SECTION),

  file_description: ($) =>
    seq(
      $.file_type,
      $.file_name,
      repeat1(choice($.fd_block, $.fd_record, $.fd_recording_mode)),
    ),

  file_type: (_) => choice(kw("FD"), kw("SD")),
  fd_block: ($) => seq(kw("BLOCK"), $.number),
  fd_record: ($) => seq(kw("RECORD"), $.number),
  fd_recording_mode: (_) => seq(kw("RECORDING"), /[A-Z]/),

  record_description: ($) => $.data_description,

  // ╭──────────────────────────────────────────────────────────╮
  // │                 WORKING STORAGE SECTION                  │
  // ╰──────────────────────────────────────────────────────────╯

  working_storage_section: ($) =>
    seq(
      seq(field("section_header", $.working_storage_section_header), "."),
      repeat(
        seq(
          choice(
            $.data_description,
            // $.copy_statement, //
          ),
          ".",
        ),
      ),
    ),
  working_storage_section_header: ($) => seq(kw("WORKING-STORAGE"), $._SECTION),

  // ╭──────────────────────────────────────────────────────────╮
  // │                     DATA DESCRIPTION                     │
  // ╰──────────────────────────────────────────────────────────╯

  data_description: ($) =>
    seq(
      $.level_number,
      $.data_name,
      repeat(
        choice(
          $.picture_clause,
          // $.redefines_clause,
          // $.usage_clause,
          // $.occurs_clause,
          // $.value_clause,
        ),
      ),
    ),
  data_name: ($) => choice(kw("FILLER"), $.variable),
  level_number: (_) => /[0-9][0-9]?/,

  picture_clause: ($) => seq(kw("PIC"), $._pic_string),

  _pic_string: ($) => choice($.pic_x),

  pic_x: (_) => /[xX](\([0-9]+\))?/,

  picture_9: ($) => choice($._picture_9_z, $._picture_9_v_1, $._picture_9_v_2),
  _picture_9_z: (_) => /[sS]?(9(\([0-9]+\))?)+([zZ](\([0-9]+\))?)+/,
  _picture_9_v_1: (_) =>
    /[sS]?([pP9](\([0-9]+\))?)+([vV]([pP9](\([0-9]+\))?)*)?/,
  _picture_9_v_2: (_) => /[sS]?[vV]([pP9](\([0-9]+\))?)*/,
};
