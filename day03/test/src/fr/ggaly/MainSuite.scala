package fr.ggaly

import munit.FunSuite

class MainSuite extends FunSuite:
  private val program =
    "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

  test("part 1") {
    assertEquals(part1(program), 161)
  }

  test("part 2") {
    assertEquals(part2(program), 48)
  }
end MainSuite
