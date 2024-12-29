module.exports = {
  // select_statement: ($) =>
  //   seq(
  //     kw("SELECT"),
  //     field("file", $.file_name),
  //     seq($._select_clause, "."), //
  //   ),

  // SELECT DEB648  ASSIGN  TO  DEB648E
  //        ORGANIZATION IS RELATIVE
  //        ACCESS IS RANDOM
  //        RELATIVE KEY IS DBK841-CH-ACESSO-648.

  // ╾───────────────────────────────────────────────────────────────────────────────────╼
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
        seq(kw("RECORD"), optional(kw("BINARY")), $._SEQUENTIAL),
        $._SEQUENTIAL,
        $._RELATIVE,
      ),
    ),

  relative_key_clause: ($) =>
    seq($._RELATIVE, op(kw("KEY")), op($._IS), field("relative_key", $.WORD)),
};
