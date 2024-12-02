package fr.ggaly

import munit.FunSuite

class MainSuite extends FunSuite:
  private val list1 = List(3, 4, 2, 1, 3, 3)
  private val list2 = List(4, 3, 5, 3, 9, 3)

  test("part 1") {
    assertEquals(part1(list1, list2), 11)
  }

  test("part 2") {
    assertEquals(part2(list1, list2), 31)
  }
end MainSuite
