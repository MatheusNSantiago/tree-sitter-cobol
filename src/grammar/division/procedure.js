module.exports = {
  procedure_division: ($) =>
    seq(
      field("division_header", $.procedure_division_header),
      op($.procedure_using),
      ".",
      repeat(
        choice(
          $.empty_sentence,
          $._sentence,
          $.paragraph,
          $.section,
          prec(3, alias($._copy_include_statement, $.copy_statement)),
        ),
      ),
    ),

  _sentence: ($) => seq(repeat1($._statement), ".", C($)),
  empty_sentence: (_) => ".",

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
        repeat(
          choice(
            $.empty_sentence,
            $._sentence,
            $.paragraph,
            prec(3, alias($._copy_include_statement, $.copy_statement)),
          ),
        ),
      ),
    ),

  // ╭──────────────────────────────────────────────────────────╮
  // │                        Paragraphs                        │
  // ╰──────────────────────────────────────────────────────────╯

  paragraph: ($) =>
    prec.right(
      seq(
        field("paragraph_header", $.paragraph_header), //
        repeat(
          choice(
            $.empty_sentence,
            $._sentence,
            prec(3, alias($._copy_include_statement, $.copy_statement)),
          ),
        ),
      ),
    ),
};
