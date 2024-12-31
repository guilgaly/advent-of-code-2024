package fr.ggaly

import munit.FunSuite

class MainSuite extends FunSuite:
  val (testGrid, testAntennas) = parseInput(
    List(
      "............",
      "........0...",
      ".....0......",
      ".......0....",
      "....0.......",
      "......A.....",
      "............",
      "............",
      "........A...",
      ".........A..",
      "............",
      "............",
    ),
  )

  test("part 1") {
    assertEquals(part1(testGrid, testAntennas), 14)
  }

  test("part 2") {
    assertEquals(part2(testGrid, testAntennas), 34)
  }
end MainSuite
