module.exports = {
  open_statement: ($) =>
    seq(
      kw("OPEN"),
      repeat1(seq(choice(kw("INPUT"), kw("OUTPUT")), repeat1($.file_name))),
      ".",
    ),
};
