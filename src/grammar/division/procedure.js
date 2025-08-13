module.exports = {
  procedure_division: ($) =>
    seq(
      field("division_header", $.procedure_division_header),
      op($.procedure_using),
      ".",
      repeat(
        prec(
          20, // Mais precedencia que um statement ($._statements)
          choice($.copy_statement, $._statement, $.paragraph, $.section),
        ),
      ),
    ),

  procedure_division_header: ($) => seq(kw("PROCEDURE"), $._DIVISION),

  procedure_using: ($) => seq($._USING, repeat1($.variable)),

  // ╭──────────────────────────────────────────────────────────╮
  // │                        Sections                          │
  // ╰──────────────────────────────────────────────────────────╯

  section: ($) =>
    prec.right(
      seq(
        field("section_header", $.section_header),
        C($),
        repeat(choice($._statement, $.paragraph)),
      ),
    ),

  // section_header: ($) => seq(/[A-Z0-9-]+/, $._SECTION, "."),

  // NOTE: Section comments seria bom pra fazer tipo um docstring, mas não sei se vale a pena

  // ╭──────────────────────────────────────────────────────────╮
  // │                        Paragraphs                        │
  // ╰──────────────────────────────────────────────────────────╯

  paragraph: ($) =>
    prec.right(
      seq(
        field("paragraph_header", $.paragraph_header), //
        C($),
        repeat($._statement),
      ),
    ),
  // paragraph_header: ($) => seq(/[A-Z0-9]+/, "."),
};
