module.exports = {
  search_statement: ($) => choice($._search_block, $._search_sentence),
  //  ╭──────────────────────────────────────────────────────────╮
  //  │                          BLOCO                           │
  //  ╰──────────────────────────────────────────────────────────╯
  _search_block: ($) =>
    prec.right(
      seq(
        seq($._SEARCH, op($._ALL)),
        field("table_name", $.variable),
        opseq($._VARYING, $.WORD),
        opseq($._AT, $._END, $.statement_block),
        repeat1(
          seq(
            $._WHEN,
            field("condition", $.expr),
            field("body", $.statement_block),
          ),
        ),
        $._END_SEARCH,
      ),
    ),

  //  ╭──────────────────────────────────────────────────────────╮
  //  │                         SENTENÇA                         │
  //  ╰──────────────────────────────────────────────────────────╯
  _search_sentence: ($) =>
    prec.right(
      seq(
        seq($._SEARCH, op($._ALL)),
        field("table_name", $.variable),
        op(seq($._VARYING, $.WORD)),
        opseq($._AT, $._END, $.atomic_imperative_block),
        repeat1(
          seq(
            $._WHEN,
            field("condition", $.expr),
            field("when_block", $.atomic_imperative_block),
          ),
        ),
      ),
    ),
};

