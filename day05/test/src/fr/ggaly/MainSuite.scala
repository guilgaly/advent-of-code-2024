package fr.ggaly

import munit.FunSuite

class MainSuite extends FunSuite:
  private val rules = List(
    (47, 53),
    (97, 13),
    (97, 61),
    (97, 47),
    (75, 29),
    (61, 13),
    (75, 53),
    (29, 13),
    (97, 29),
    (53, 29),
    (61, 53),
    (97, 53),
    (61, 29),
    (47, 13),
    (75, 47),
    (97, 75),
    (47, 61),
    (75, 61),
    (47, 29),
    (75, 13),
    (53, 13),
  )
  private val updates = List(
    List(75, 47, 61, 53, 29),
    List(97, 61, 53, 29, 13),
    List(75, 29, 13),
    List(75, 97, 47, 61, 53),
    List(61, 13, 29),
    List(97, 13, 75, 29, 47),
  )

  test("part 1") {
    assertEquals(part1(rules, updates), 143)
  }

  test("part 2") {
    assertEquals(part2(rules, updates), 123)
  }
end MainSuite
