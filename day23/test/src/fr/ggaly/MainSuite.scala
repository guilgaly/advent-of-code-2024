package fr.ggaly

import munit.FunSuite

class MainSuite extends FunSuite:
  private val testInput = parseInput(
    List(
      "kh-tc",
      "qp-kh",
      "de-cg",
      "ka-co",
      "yn-aq",
      "qp-ub",
      "cg-tb",
      "vc-aq",
      "tb-ka",
      "wh-tc",
      "yn-cg",
      "kh-ub",
      "ta-co",
      "de-co",
      "tc-td",
      "tb-wq",
      "wh-td",
      "ta-ka",
      "td-qp",
      "aq-cg",
      "wq-ub",
      "ub-vc",
      "de-ta",
      "wq-aq",
      "wq-vc",
      "wh-yn",
      "ka-de",
      "kh-ta",
      "co-tc",
      "wh-qp",
      "tb-vc",
      "td-yn",
    ),
  )

  test("part 1") {
    assertEquals(part1(testInput), 7)
  }

  test("part 2") {
    assertEquals(part2(testInput), "co,de,ka,ta")
  }
end MainSuite
