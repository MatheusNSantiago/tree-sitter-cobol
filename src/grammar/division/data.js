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
          $._EJECT,
        ),
      ),
    ),

  data_division_header: ($) => seq($._DATA, $._DIVISION),

  _data_division_statements: ($) =>
    seq(choice($.exec_sql, $.data_description, $.copy_statement), "."),

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
      $._BLOCK,
      op($._CONTAINS),
      field("num", $.number),
      op(choice($._CHARACTERS, $._RECORDS)),
    ),

  fd_record: ($) =>
    seq(
      $._RECORD,
      optional($._CONTAINS),
      field("num", $.integer),
      field("to", opseq($._TO, $.integer)),
      optional($._CHARACTERS),
    ),

  fd_recording_mode: ($) =>
    seq($._RECORDING, op($._MODE), op($._IS), field("mode", $.WORD)),

  fd_label_records: ($) =>
    seq(
      $._LABEL,
      choice(seq($._RECORD, op($._IS)), seq($._RECORDS, op($._ARE))),
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
      repeat($._data_division_statements),
    ),
  working_storage_section_header: ($) => seq($._WORKING_STORAGE, $._SECTION),

  data_description: ($) =>
    seq(
      field("level", $.level_number),
      field("name", $.data_name),
      choice(
        seq($._REDEFINES, field("redefines", $.variable)),
        $._pic_value, // level 88
        field("occurs", $.occurs),
        seq(
          $._PIC,
          choice(
            $._INDEX,
            repeat(
              choice(
                $._pic_value,
                field("def", choice($.pic_x, $.pic_9, $.pic_a, $.pic_edit)),
                field("comp", $.comp),
                field("occurs", $.occurs),
              ),
            ),
          ),
        ),
      ),
    ),

  data_name: ($) => choice($._FILLER, $.variable),
  level_number: (_) => /[0-9][0-9]?/,

  // ╾───────────────────────────────────────────────────────────────────────────────────╼

  pic_x: (_) => /[xX]+(\([0-9]+\))?/,
  pic_9: (_) => /[sS]?9+(\([0-9]+\))?([vV](9(\([0-9]+\))?)*)?/,
  pic_a: (_) => /([aA](\([0-9]+\))?)+/,
  pic_edit: (_) =>
    /([aAxX9bBvVzZpPwW\(\)0-9$/,\.*+<>-]|[cC][rR]|[dD][bB])*([aAxX9bBvVzZpPwW\(\)0-9$/,*+<>-]|[cC][rR]|[dD][bB])/,

  comp: ($) =>
    seq(
      opseq($._USAGE, optional($._IS)),
      choice(
        $._COMP,
        $._COMP_1,
        $._COMP_2,
        $._COMP_3,
        $._COMP_4,
        $._COMP_5,
        $._COMPUTATIONAL,
        $._COMPUTATIONAL_1,
        $._COMPUTATIONAL_2,
        $._COMPUTATIONAL_3,
        $._COMPUTATIONAL_4,
        $._COMPUTATIONAL_5,
      ),
    ),
  // ╾───────────────────────────────────────────────────────────────────────────────────╼

  _pic_value: ($) =>
    seq(
      choice($._VALUE, $._VALUES),
      op($._ALL),
      field("value", repeat1($.value_item)),
    ),

  value_item: ($) => seq($._value, opseq($._THRU, $._value)),

  occurs: ($) =>
    seq(
      seq($._OCCURS, field("times", $.number)),
      opseq($._TO, field("to", $.number)),
      op($._TIMES),
      field("depending", opseq($._DEPENDING, op($._ON), $.variable)),
      field("key_spec", op($.occurs_key_spec)),
      op($.indexed_by),
      // op(choice($._picture, $.indexed_by)),
    ),

  occurs_key_spec: ($) =>
    prec.left(
      2,
      choice(
        seq(repeat1($.sort_key), optional($.indexed_by)),
        seq($.indexed_by, repeat($.sort_key)),
      ),
    ),

  sort_key: ($) =>
    seq(
      field("order", choice($._ASCENDING, $._DESCENDING)),
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
      repeat($._data_division_statements),
    ),
  local_storage_section_header: ($) => seq(kw("LOCAL-STORAGE"), $._SECTION),

  // ╭──────────────────────────────────────────────────────────╮
  // │                     Linkage Section                      │
  // ╰──────────────────────────────────────────────────────────╯

  linkage_section: ($) =>
    seq(
      seq(field("section_header", $.linkage_section_header), "."),
      C($),
      repeat($._data_division_statements),
    ),
  linkage_section_header: ($) => seq(kw("LINKAGE"), $._SECTION),
};
