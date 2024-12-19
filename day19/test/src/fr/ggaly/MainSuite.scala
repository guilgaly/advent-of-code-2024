package fr.ggaly

import munit.FunSuite

class MainSuite extends FunSuite:
  private val testTowels = List("r", "wr", "b", "g", "bwu", "rb", "gb", "br")
  private val testPatterns = List(
    "brwrr",
    "bggr",
    "gbbr",
    "rrbgbr",
    "ubwu",
    "bwurrg",
    "brgr",
    "bbrgwb",
  )

  test("part 1") {
    assertEquals(part1(testTowels, testPatterns), 6)
  }

  test("part 2") {
    assertEquals(part2(testTowels, testPatterns), 16L)
  }
end MainSuite
