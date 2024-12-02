package fr.ggaly

import munit.FunSuite

class MainSuite extends FunSuite:
  private val reports = List(
    List(7, 6, 4, 2, 1),
    List(1, 2, 7, 8, 9),
    List(9, 7, 6, 2, 1),
    List(1, 3, 2, 4, 5),
    List(8, 6, 4, 4, 1),
    List(1, 3, 6, 7, 9),
  )

  test("part 1") {
    assertEquals(part1(reports), 2)
  }

  test("part 2") {
    assertEquals(part2(reports), 4)
  }
end MainSuite
