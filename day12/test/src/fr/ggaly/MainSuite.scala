package fr.ggaly

import munit.FunSuite

class MainSuite extends FunSuite:
  private val testInput = Farm(
    Array(
      "RRRRIICCFF".toArray,
      "RRRRIICCCF".toArray,
      "VVRRRCCFFF".toArray,
      "VVRCCCJFFF".toArray,
      "VVVVCJJCFE".toArray,
      "VVIVCCJJEE".toArray,
      "VVIIICJJEE".toArray,
      "MIIIIIJJEE".toArray,
      "MIIISIJEEE".toArray,
      "MMMISSJEEE".toArray,
    ),
  )

  test("part 1") {
    assertEquals(part1(testInput), 1930)
  }

  test("part 2") {
    assertEquals(part2(testInput), 1206)
  }
end MainSuite
