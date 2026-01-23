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
    prec.right(seq(
      choice(
        seq($.exec_sql, op(".")),
        seq($.data_description, "."),
        seq($.copy_statement, "."),
      ),
      C($),
    )),

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
  // BLOCK [CONTAINS] num [CHARACTERS|RECORDS]
  _BLOCK_CONTAINS: (_) => /[bB][lL][oO][cC][kK]([ \t]+[cC][oO][nN][tT][aA][iI][nN][sS])?/,
  fd_block: ($) =>
    seq(
      $._BLOCK_CONTAINS,
      field("num", $.number),
      optional(choice($._CHARACTERS, $._RECORDS)),
    ),

  // RECORD [CONTAINS] num [TO num] [CHARACTERS]
  _RECORD_CONTAINS: (_) => /[rR][eE][cC][oO][rR][dD]([ \t]+[cC][oO][nN][tT][aA][iI][nN][sS])?/,
  fd_record: ($) =>
    seq(
      $._RECORD_CONTAINS,
      field("num", $.integer),
      field("to", opseq($._TO, $.integer)),
      optional($._CHARACTERS),
    ),

  // RECORDING [MODE] [IS] mode - consolidado em regex
  _RECORDING_MODE: (_) => /[rR][eE][cC][oO][rR][dD][iI][nN][gG]([ \t]+[mM][oO][dD][eE])?([ \t]+[iI][sS])?/,
  fd_recording_mode: ($) => seq($._RECORDING_MODE, field("mode", $.WORD)),

  // LABEL RECORD[S] [IS|ARE] STANDARD|OMITTED
  _LABEL_RECORDS: (_) => /[lL][aA][bB][eE][lL][ \t]+[rR][eE][cC][oO][rR][dD][sS]?([ \t]+([iI][sS]|[aA][rR][eE]))?/,
  fd_label_records: ($) =>
    seq(
      $._LABEL_RECORDS,
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
      field("name", optional($.data_name)), // opcional para FILLER implícito
      optional(
        choice(
          seq($._REDEFINES, field("redefines", $.variable)),
          $._pic_value, // level 88
          field("occurs", $.occurs),
          // COMP-1/COMP-2 sem PIC (floating point)
          seq(
            optional($._pic_value),
            field("comp", $.comp),
            optional($._pic_value),
          ),
          // PIC clause (VALUE pode vir antes ou depois)
          seq(
            optional($._pic_value), // VALUE antes do PIC
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
    ),

  data_name: ($) => choice($._FILLER, $.variable),
  level_number: (_) => /[0-9][0-9]?/,

  // ╾───────────────────────────────────────────────────────────────────────────────────╼

  pic_x: (_) => /[xX]+(\([0-9]+\))?/,
  pic_9: (_) => /[sS]?9+(\([0-9]+\))?([vV](9(\([0-9]+\))?)*)?/,
  pic_a: (_) => /([aA](\([0-9]+\))?)+/,
  pic_edit: (_) =>
    /([aAxX9bBvVzZpPwW\(\)0-9$/,\.*+<>-]|[cC][rR]|[dD][bB])*([aAxX9bBvVzZpPwW\(\)0-9$/,*+<>-]|[cC][rR]|[dD][bB])/,

  // [USAGE [IS]] COMP-n - USAGE IS é opcional
  _USAGE_IS: (_) => /[uU][sS][aA][gG][eE]([ \t]+[iI][sS])?[ \t]+/,
  comp: ($) =>
    seq(
      optional($._USAGE_IS),
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

  // DEPENDING [ON] consolidado em regex
  _DEPENDING_ON: (_) => /[dD][eE][pP][eE][nN][dD][iI][nN][gG]([ \t]+[oO][nN])?/,
  occurs: ($) =>
    seq(
      seq($._OCCURS, field("times", $.number)),
      opseq($._TO, field("to", $.number)),
      op($._TIMES),
      field("depending", opseq($._DEPENDING_ON, $.variable)),
      field("key_spec", op($.occurs_key_spec)),
      op($.indexed_by),
    ),

  occurs_key_spec: ($) =>
    prec.left(
      2,
      choice(
        seq(repeat1($.sort_key), optional($.indexed_by)),
        seq($.indexed_by, repeat($.sort_key)),
      ),
    ),

  // ASCENDING/DESCENDING [KEY] [IS] - consolidado
  _ASCENDING_KEY: (_) => /[aA][sS][cC][eE][nN][dD][iI][nN][gG]([ \t]+[kK][eE][yY])?([ \t]+[iI][sS])?/,
  _DESCENDING_KEY: (_) => /[dD][eE][sS][cC][eE][nN][dD][iI][nN][gG]([ \t]+[kK][eE][yY])?([ \t]+[iI][sS])?/,
  sort_key: ($) =>
    seq(
      field("order", choice($._ASCENDING_KEY, $._DESCENDING_KEY)),
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
