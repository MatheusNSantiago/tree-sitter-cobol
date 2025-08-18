module.exports = {
  procedure_division: ($) =>
    seq(
      field("division_header", $.procedure_division_header),
      op($.procedure_using),
      ".",
      repeat(choice($.sentence, $.paragraph, $.section)),
    ),

  sentence: ($) => seq(repeat1($._statement), "."),

  procedure_division_header: ($) => seq($._PROCEDURE, $._DIVISION),

  procedure_using: ($) => seq($._USING, repeat1($.variable)),

  // ╭──────────────────────────────────────────────────────────╮
  // │                        Sections                          │
  // ╰──────────────────────────────────────────────────────────╯

  section: ($) =>
    prec.right(
      seq(
        field("section_header", $.section_header),
        repeat(choice($.sentence, $.paragraph)),
      ),
    ),

  // ╭──────────────────────────────────────────────────────────╮
  // │                        Paragraphs                        │
  // ╰──────────────────────────────────────────────────────────╯

  paragraph: ($) =>
    prec.right(
      seq(
        field("paragraph_header", $.paragraph_header), //
        repeat($.sentence),
      ),
    ),
};
