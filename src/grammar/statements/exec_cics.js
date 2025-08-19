module.exports = {
  // ╭──────────────────────────────────────────────────────────╮
  // │                        EXEC CICS                         │
  // ╰──────────────────────────────────────────────────────────╯
  exec_cics: ($) =>
    seq(
      seq($._EXEC, kw("CICS"), op(field("operation", $.cics_operation))),
      repeat($._exec_cics_body),
      $._END_EXEC,
    ),

  cics_operation: ($) =>
    choice(
      kw("PUT"),
      $._ASSIGN,
      kw("GET"),
      kw("LINK"),
      kw("DELETE"),
      kw("SYNCPOINT"),
    ),

  _exec_cics_body: ($) => prec.right(15, seq($._exec_cics_statements, C($))),
  _exec_cics_statements: ($) =>
    choice(
      field("program", seq(kw("PROGRAM"), paren(choice($.variable, $.string)))),
      field("commarea", seq(kw("COMMAREA"), paren($.variable))),
      field(
        "container",
        seq(kw("CONTAINER"), paren(choice($.string, $.variable))),
      ),
      field("into", seq($._INTO, paren($.variable))),
      seq($._FROM, paren($.variable)),
      seq(kw("CHANNEL"), paren($.variable)),
      seq(kw("RESP"), paren($.variable)),
      seq(kw("RESP2"), paren($.variable)),
      seq(
        choice($._LENGTH, "FLENGTH"),
        paren(choice(seq($._LENGTH, $._OF, $.variable), $.variable)),
      ),
      kw("RETURN"),
      kw("NOHANDLE"),
      kw("NODATA"),
      kw("ROLLBACK"),
    ),

  // ╾───────────────────────────────────────────────────────────────────────────────────╼
  _OF: () => kw("OF"),
  _FROM: () => kw("FROM"),
  _FOR: () => kw("FOR"),
  _INTO: () => kw("INTO"),
  SELECT: () => kw("SELECT"),
  INSERT: () => kw("INSERT"),
  UPDATE: () => kw("UPDATE"),
  DELETE: () => kw("DELETE"),
};
