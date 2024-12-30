module.exports = {
  select_statement: ($) =>
    seq(
      kw("SELECT"),
      field("file", $.file_name),
      repeat($._select_clause),
      ".",
    ),

  _select_clause: ($) =>
    choice(
      $.assign_clause,
      $.access_mode_clause,
      $.organization_clause,
      $.relative_key_clause,
      $.file_status_clause,
      $.record_key_clause,
    ),

  assign_clause: ($) => seq(kw("ASSIGN"), $._TO, field("dd_name", $.WORD)),

  access_mode_clause: ($) =>
    seq(
      kw("ACCESS"),
      op(kw("MODE")),
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
