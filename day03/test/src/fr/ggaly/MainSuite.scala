package fr.ggaly

import munit.FunSuite

class MainSuite extends FunSuite:
  private val program =
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

  test("part 1") {
    assertEquals(part1(program), 161)
  }

  test("part 2") {}
end MainSuite
