module.exports = {
  data_division: ($) =>
    seq(
      seq(field("division_header", $.data_division_header), "."),
      repeat(
        choice(
          $.file_section,
          $.working_storage_section,
          $.local_storage_section,
          $.linkage_section,
        ),
      ),
    ),

  data_division_header: ($) => seq(kw("DATA"), $._DIVISION),

  // ╭──────────────────────────────────────────────────────────╮
  // │                       FILE SECTION                       │
  // ╰──────────────────────────────────────────────────────────╯

  file_section: ($) =>
    seq(
      seq(field("section_header", $.file_section_header), "."),
      C($),
      repeat(
        seq(
          choice(
            $.file_description,
            $.record_description,
            $.copy_statement, //
          ),
          ".",
          C($),
        ),
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
      C($),
      repeat(
        seq(
          choice($.data_description, $.copy_statement, $._exec_statement),
          ".",
          C($),
        ),
      ),
    ),
  working_storage_section_header: ($) => seq(kw("WORKING-STORAGE"), $._SECTION),

  data_description: ($) =>
    seq(
      $.level_number,
      $.data_name,
      optional(
        // optional pq pode ser um lider de group
        choice(
          $.picture,
          $.redefines, // 01 WS-RECORD-1 REDEFINES WS-RECORD-2
          $.occurs, // 01 WS-RECORD OCCURS 10 TIMES
          kw("INDEX"), // 77 IDX-601F-LIM     INDEX.
          $.pic_value, // 88 CND-STATUS-OK VALUE 0
        ),
      ),
    ),
  data_name: ($) => choice(kw("FILLER"), $.variable),
  level_number: (_) => /[0-9][0-9]?/,

  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  picture: ($) =>
    seq(kw("PIC"), $._pic_string, optional($.comp), optional($.pic_value)),
  _pic_string: ($) => choice($.pic_x, $.pic_9),

  pic_x: (_) => /[xX]+(\([0-9]+\))?/,
  pic_9: (_) => /[sS]?9+(\([0-9]+\))?([vV]9+(\([0-9]+\))?)?/,

  comp: (_) =>
    choice(
      kw("COMP"),
      kw("COMP-1"),
      kw("COMP-2"),
      kw("COMP-3"),
      kw("COMP-4"),
      kw("COMP-5"),
    ),
  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  redefines: ($) => seq(kw("REDEFINES"), $.variable),

  pic_value: ($) => seq(kw("VALUE"), repeat1($._value)),

  occurs: ($) =>
    seq(
      kw("OCCURS"),
      $.number,
      kw("TIMES"),
      optional(choice($.picture, $.indexed_by)),
    ),
  indexed_by: ($) => seq(kw("INDEXED"), kw("BY"), $.variable),

  // ╭──────────────────────────────────────────────────────────╮
  // │                  LOCAL STORAGE SECTION                   │
  // ╰──────────────────────────────────────────────────────────╯

  local_storage_section: ($) =>
    seq(
      seq(field("section_header", $.local_storage_section_header), "."),
      C($),
      repeat(
        seq(
          choice(
            seq($._exec_statement, "."),
            seq($.data_description, "."),
            $.copy_statement,
          ),
          C($),
        ),
      ),
    ),
  local_storage_section_header: ($) => seq(kw("LOCAL-STORAGE"), $._SECTION),

  // ╭──────────────────────────────────────────────────────────╮
  // │                     Linkage Section                      │
  // ╰──────────────────────────────────────────────────────────╯

  linkage_section: ($) =>
    seq(
      seq(field("section_header", $.linkage_section_header), "."),
      C($),
      repeat(
        seq(
          choice(seq($.data_description, "."), $.copy_statement), //
          C($),
        ),
      ),
    ),
  linkage_section_header: ($) => seq(kw("LINKAGE"), $._SECTION),
};
