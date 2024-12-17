package fr.ggaly

import munit.FunSuite

class MainSuite extends FunSuite:
  test("If register C contains 9, the program 2,6 would set register B to 1.") {
    assertEquals(Computer(0, 0, 9, Array(2, 6)).run.registers.b, 1L)
  }

  test("If register A contains 10, the program 5,0,5,1,5,4 would output 0,1,2.") {
    assertEquals(Computer(10, 0, 0, Array(5, 0, 5, 1, 5, 4)).run.readOutput, List(0L, 1L, 2L))

  }

  test(
    "If register A contains 2024, the program 0,1,5,4,3,0 would output 4,2,5,6,7,7,7,7,3,1,0 and leave 0 in register A.",
  ) {
    val endState = Computer(2024, 0, 0, Array(0, 1, 5, 4, 3, 0)).run
    assertEquals(endState.readOutput, List(4L, 2L, 5L, 6L, 7L, 7L, 7L, 7L, 3L, 1L, 0L))
    assertEquals(endState.registers.a, 0L)
  }

  test("If register B contains 29, the program 1,7 would set register B to 26.") {
    assertEquals(Computer(0, 29, 0, Array(1, 7)).run.registers.b, 26L)
  }

  test(
    "If register B contains 2024 and register C contains 43690, the program 4,0 would set register B to 44354.",
  ) {
    assertEquals(Computer(0, 2024, 43690, Array(4, 0)).run.registers.b, 44354L)
  }

  test("part 1") {
    assertEquals(part1(Computer(729, 0, 0, Array(0, 1, 5, 4, 3, 0))), "4,6,3,5,6,3,5,2,1,0")
  }

  test("part 2") {
    assertEquals(part2(Computer(2024, 0, 0, Array(0, 3, 5, 4, 3, 0))), 117440L)
  }
end MainSuite
