package fr.ggaly

import munit.FunSuite

class MainSuite extends FunSuite:
  val testInput = List(
    "....#.....",
    ".........#",
    "..........",
    "..#.......",
    ".......#..",
    "..........",
    ".#..^.....",
    "........#.",
    "#.........",
    "......#...",
  )

  test("part 1") {
    assertEquals(part1(testInput), 41)
  }

  test("part 2") {
    assertEquals(part2(testInput), 6)
  }
end MainSuite
