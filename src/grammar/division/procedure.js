module.exports = {
  procedure_division: ($) =>
    seq(
      field("division_header", $.procedure_division_header),
      op($.procedure_using),
      ".",
      repeat(choice($._sentence, $.paragraph, $.section)),
    ),

  _sentence: ($) => seq(repeat1($._statement), "."),
  // _sentence: ($) => prec.right(seq(repeat1($._statement), op("."))),

  procedure_division_header: ($) => seq($._PROCEDURE, $._DIVISION),

  procedure_using: ($) => seq($._USING, repeat1($.variable)),

  // ╭──────────────────────────────────────────────────────────╮
  // │                        Sections                          │
  // ╰──────────────────────────────────────────────────────────╯

  section: ($) =>
    prec.right(
      seq(
        field("section_header", $.section_header),
        repeat(choice($._sentence, $.paragraph)),
      ),
    ),

  // ╭──────────────────────────────────────────────────────────╮
  // │                        Paragraphs                        │
  // ╰──────────────────────────────────────────────────────────╯

  paragraph: ($) =>
    prec.right(
      seq(
        field("paragraph_header", $.paragraph_header), //
        repeat($._sentence),
      ),
    ),
};
