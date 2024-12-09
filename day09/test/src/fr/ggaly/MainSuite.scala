package fr.ggaly

import munit.FunSuite

class MainSuite extends FunSuite:
  val testInput = "2333133121414131402"

  test("part 1") {
    assertEquals(part1(testInput), 1928L)
  }

  test("part 2") {
    assertEquals(part2(testInput), 2858L)
  }
end MainSuite
