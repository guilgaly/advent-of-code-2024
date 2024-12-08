package fr.ggaly

import munit.FunSuite

class MainSuite extends FunSuite:
  private val testInput = List(
    "............",
    "........0...",
    ".....0......",
    ".......0....",
    "....0.......",
    "......A.....",
    "............",
    "............",
    "........A...",
    ".........A..",
    "............",
    "............",
  )

  test("part 1") {
    assertEquals(part1(testInput), 14)
  }

  test("part 2") {
    assertEquals(part2(testInput), 34)
  }
end MainSuite
