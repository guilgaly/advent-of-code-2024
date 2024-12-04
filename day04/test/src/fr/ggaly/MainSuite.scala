package fr.ggaly

import munit.FunSuite

class MainSuite extends FunSuite:
  private val testInput = Array(
    "MMMSXXMASM".toCharArray,
    "MSAMXMSMSA".toCharArray,
    "AMXSXMAAMM".toCharArray,
    "MSAMASMSMX".toCharArray,
    "XMASAMXAMM".toCharArray,
    "XXAMMXXAMA".toCharArray,
    "SMSMSASXSS".toCharArray,
    "SAXAMASAAA".toCharArray,
    "MAMMMXMMMM".toCharArray,
    "MXMXAXMASX".toCharArray,
  )

  test("part 1") {
    assertEquals(part1(testInput), 18)
  }

  test("part 2") {
    assertEquals(part2(testInput), 9)
  }
end MainSuite
