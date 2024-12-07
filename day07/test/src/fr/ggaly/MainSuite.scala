package fr.ggaly

import munit.FunSuite

class MainSuite extends FunSuite:
  private val testInput = List(
    Equation(190, List(10, 19)),
    Equation(3267, List(81, 40, 27)),
    Equation(83, List(17, 5)),
    Equation(156, List(15, 6)),
    Equation(7290, List(6, 8, 6, 15)),
    Equation(161011, List(16, 10, 13)),
    Equation(192, List(17, 8, 14)),
    Equation(21037, List(9, 7, 18, 13)),
    Equation(292, List(11, 6, 16, 20)),
  )

  test("part 1") {
    assertEquals(part1(testInput), 3749L)
  }

  test("part 2") {
    assertEquals(part2(testInput), 11387L)
  }
end MainSuite
