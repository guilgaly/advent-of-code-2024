package fr.ggaly

import munit.FunSuite

class MainSuite extends FunSuite:
  private val (testLocks, testKeys) = parseInput(
    """#####
      |.####
      |.####
      |.####
      |.#.#.
      |.#...
      |.....
      |
      |#####
      |##.##
      |.#.##
      |...##
      |...#.
      |...#.
      |.....
      |
      |.....
      |#....
      |#....
      |#...#
      |#.#.#
      |#.###
      |#####
      |
      |.....
      |.....
      |#.#..
      |###..
      |###.#
      |###.#
      |#####
      |
      |.....
      |.....
      |.....
      |#....
      |#.#..
      |#.#.#
      |#####""".stripMargin,
  )

  test("part 1") {
    assertEquals(part1(testLocks, testKeys), 3)
  }

  test("part 2") {}
end MainSuite
