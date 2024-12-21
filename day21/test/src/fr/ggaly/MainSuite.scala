package fr.ggaly

import munit.FunSuite

class MainSuite extends FunSuite:
  private val testInput = parseInput(
    List(
      "029A",
      "980A",
      "179A",
      "456A",
      "379A",
    ),
  )

  test("part 1") {
    assertEquals(part1(testInput.slice(0, 1)), 68 * 29)
    assertEquals(part1(testInput.slice(1, 2)), 60 * 980)
    assertEquals(part1(testInput.slice(2, 3)), 68 * 179)
    assertEquals(part1(testInput.slice(3, 4)), 64 * 456)
    assertEquals(part1(testInput.slice(4, 5)), 64 * 379)
    assertEquals(part1(testInput), 126384)
  }

  test("part 2") {}
end MainSuite
