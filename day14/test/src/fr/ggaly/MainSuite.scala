package fr.ggaly

import munit.FunSuite

class MainSuite extends FunSuite:
  private val testInput = parseInput(
    List(
      "p=0,4 v=3,-3",
      "p=6,3 v=-1,-3",
      "p=10,3 v=-1,2",
      "p=2,0 v=2,-1",
      "p=0,0 v=1,3",
      "p=3,0 v=-2,-2",
      "p=7,6 v=-1,-3",
      "p=3,0 v=-1,-2",
      "p=9,3 v=2,3",
      "p=7,3 v=-1,2",
      "p=2,4 v=2,-3",
      "p=9,5 v=-3,-3",
    ),
  )

  private val testSpace = Space(11, 7)

  test("part 1") {
    assertEquals(part1(testSpace, testInput), 12)
  }

  test("part 2") {}
end MainSuite
