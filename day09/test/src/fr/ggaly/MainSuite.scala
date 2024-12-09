package fr.ggaly

import munit.FunSuite

class MainSuite extends FunSuite:
  val testInput = "2333133121414131402"

  test("part 1") {
    assertEquals(part1(testInput), 1928L)
  }

  test("part 2 (naive)") {
    assertEquals(part2Naive(testInput), 2858L)
  }

  test("part 2 (optimized)") {
    assertEquals(part2Optimized(testInput), 2858L)
  }
end MainSuite
