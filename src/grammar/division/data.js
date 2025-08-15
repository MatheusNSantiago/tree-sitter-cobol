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
          //
          $._eject,
        ),
      ),
    ),

  _eject: (_) => kw("EJECT"),
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
          op("."),
          C($),
        ),
      ),
    ),
  file_section_header: ($) => seq(kw("FILE"), $._SECTION),

  file_description: ($) =>
    seq(
      $.file_type,
      $.file_name,
      repeat1(
        choice(
          $.fd_block,
          $.fd_record,
          $.fd_recording_mode,
          $.fd_label_records,
        ),
      ),
    ),

  file_type: (_) => choice(kw("FD"), kw("SD")),
  fd_block: ($) =>
    seq(
      kw("BLOCK"),
      op($._CONTAINS),
      field("num", $.number),
      op(choice($._CHARACTERS, kw("RECORDS"))),
    ),

  fd_record: ($) =>
    seq(
      kw("RECORD"),
      optional($._CONTAINS),
      field("num", $.integer),
      field("to", optional(seq($._TO, $.integer))),
      optional($._CHARACTERS),
    ),

  fd_recording_mode: ($) =>
    seq(
      kw("RECORDING"),
      optional(kw("MODE")),
      optional($._IS),
      field("mode", $.WORD),
    ),

  fd_label_records: ($) =>
    seq(
      $._LABEL,
      choice(
        seq($._RECORD, optional($._IS)),
        seq($._RECORDS, optional($._ARE)),
      ),
      choice($._STANDARD, $._OMITTED),
    ),

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
          choice(
            seq($.exec_sql, op(".")), //
            seq($.data_description, op(".")),
            $.copy_statement,
          ),
          C($),
        ),
      ),
    ),
  working_storage_section_header: ($) => seq(kw("WORKING-STORAGE"), $._SECTION),

  data_description: ($) =>
    seq(
      field("level", $.level_number),
      field("name", $.data_name),

      // optional pq pode ser um lider de group
      optional(
        choice(
          $._picture,
          field(
            "redefines",
            seq(kw("REDEFINES"), $.variable, op($._picture)), // 01 WS-RECORD-1 REDEFINES WS-RECORD-2 <<PIC X(10)>>
          ),
          seq(op($._picture), field("occurs", $.occurs)), // 01 WS-RECORD <<PIC X(10)>> OCCURS 10 TIMES
          kw("INDEX"), // 77 IDX-601F-LIM     INDEX.
          $._pic_value, // 88 CND-STATUS-OK VALUE 0
        ),
      ),
    ),
  data_name: ($) => choice(kw("FILLER"), $.variable),
  level_number: (_) => /[0-9][0-9]?/,

  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  _picture: ($) => seq(kw("PIC"), field("type", $.pic_type), op($._pic_value)),

  pic_type: ($) =>
    seq(
      field("def", $._pic_def), //
      opseq(kw("USAGE"), optional($._IS)),
      op(field("comp", $.comp)),
    ),

  _pic_def: ($) => choice($.pic_x, $.pic_9, $.pic_a, $.pic_edit),

  pic_x: (_) => /[xX]+(\([0-9]+\))?/,
  // pic_9: (_) => /[sS]?9+(\([0-9]+\))?([vV]9+(\([0-9]+\))?)?/,
  pic_9: (_) => /[sS]?9+(\([0-9]+\))?([vV](9(\([0-9]+\))?)*)?/,

  pic_a: (_) => /([aA](\([0-9]+\))?)+/,
  pic_edit: (_) =>
    /([aAxX9bBvVzZpPwW\(\)0-9$/,\.*+<>-]|[cC][rR]|[dD][bB])*([aAxX9bBvVzZpPwW\(\)0-9$/,*+<>-]|[cC][rR]|[dD][bB])/,

  comp: (_) =>
    choice(
      kw("COMP"),
      kw("COMP-1"),
      kw("COMP-2"),
      kw("COMP-3"),
      kw("COMP-4"),
      kw("COMP-5"),
      kw("COMPUTATIONAL"),
      kw("COMPUTATIONAL-1"),
      kw("COMPUTATIONAL-2"),
      kw("COMPUTATIONAL-3"),
      kw("COMPUTATIONAL-4"),
      kw("COMPUTATIONAL-5"),
    ),
  // ╾───────────────────────────────────────────────────────────────────────────────────╼

  _pic_value: ($) =>
    seq(
      choice($._VALUE, $._VALUES),
      op($._ALL),
      repeat1(field("value", $.value_item)),
    ),
  value_item: ($) => seq($._value, opseq($._THRU, $._value)),

  occurs: ($) =>
    seq(
      kw("OCCURS"),
      field("times", $.number),
      kw("TIMES"),
      op($.occurs_key_spec),
      op(choice($._picture, $.indexed_by)),
    ),

  occurs_key_spec: ($) =>
    prec.left(
      2,
      choice(
        seq(repeat1($.occurs_key), optional($.indexed_by)),
        seq($.indexed_by, repeat($.occurs_key)),
      ),
    ),

  occurs_key: ($) =>
    seq(
      choice($._ASCENDING, $._DESCENDING),
      op($._KEY),
      op($._IS),
      field("keys", repeat1($.variable)),
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
            seq($.exec_sql, op(".")),
            seq($.data_description, op(".")),
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
          choice(
            seq($.exec_sql, op(".")),
            seq($.data_description, op(".")),
            $.copy_statement,
          ),
          C($),
        ),
      ),
    ),
  linkage_section_header: ($) => seq(kw("LINKAGE"), $._SECTION),
};
