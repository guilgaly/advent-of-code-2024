package fr.ggaly

import munit.FunSuite

class MainSuite extends FunSuite:
  private val testInput = TrailMap(
    Array(
      Array(8, 9, 0, 1, 0, 1, 2, 3),
      Array(7, 8, 1, 2, 1, 8, 7, 4),
      Array(8, 7, 4, 3, 0, 9, 6, 5),
      Array(9, 6, 5, 4, 9, 8, 7, 4),
      Array(4, 5, 6, 7, 8, 9, 0, 3),
      Array(3, 2, 0, 1, 9, 0, 1, 2),
      Array(0, 1, 3, 2, 9, 8, 0, 1),
      Array(1, 0, 4, 5, 6, 7, 3, 2),
    ),
  )

  test("part 1") {
    assertEquals(part1(testInput), 36)
  }

  test("part 2") {
    assertEquals(part2(testInput), 81)
  }
end MainSuite
