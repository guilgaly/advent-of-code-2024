package fr.ggaly

import munit.FunSuite

class MainSuite extends FunSuite:
  private val testInput = List(125L, 17L)

  test("part 1") {
    assertEquals(part1(testInput), 55312)
  }
