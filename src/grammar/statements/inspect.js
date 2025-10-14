module.exports = {
  inspect_statement: ($) =>
    seq(
      $._INSPECT,
      field("send", choice($.variable, $._value, $.function)),
      repeat1(
        choice($.inspect_tallying, $.inspect_replacing, $.inspect_converting),
      ),
    ),

  inspect_tallying: ($) =>
    seq(
      $._TALLYING,
      repeat1(
        choice(
          field("for", seq($.variable, $._FOR)),
          field("character", seq($._CHARACTERS, optional($.inspect_region))),
          field("all", $._ALL),
          field("leading", $._LEADING),
          field("trailing", $._TRAILING),
          field("two", seq($._value, optional($.inspect_region))),
        ),
      ),
    ),

  inspect_replacing: ($) => seq($._REPLACING, repeat1($.replacing_item)),

  replacing_item: ($) =>
    choice(
      field(
        "characters_by",
        seq($._CHARACTERS, $._BY, $._value, optional($.inspect_region)),
      ),
      field(
        "region",
        seq(
          optional(choice($._ALL, $._LEADING, $._FIRST, $._TRAILING)),
          $.replacing_region,
        ),
      ),
    ),

  replacing_region: ($) =>
    seq(
      field("x", $._value),
      $._BY,
      field("by", $._simple_all_value),
      field("region", optional($.inspect_region)),
    ),

  inspect_converting: ($) =>
    seq(
      $._CONVERTING,
      field("x", $._value),
      $._TO,
      field("to", $._simple_all_value),
      field("region", optional($.inspect_region)),
    ),

  inspect_region: ($) =>
    repeat1(
      seq(choice($._BEFORE, $._AFTER), optional($._INITIAL), field("x", $.variable)),
    ),

  _simple_all_value: ($) => choice($.variable, $._value),
};
