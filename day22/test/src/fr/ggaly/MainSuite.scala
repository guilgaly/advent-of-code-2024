package fr.ggaly

import munit.FunSuite

class MainSuite extends FunSuite:
  test("primitives") {
    assertEquals(mix(42L, 15L), 37L)
    assertEquals(prune(100000000L), 16113920L)
    assertEquals(nextSecret(123L), 15887950L)
    assertEquals(nextSecret(15887950L), 16495136L)
    assertEquals(nextSecret(16495136L), 527345L)
    assertEquals(nextSecret(527345L), 704524L)
  }

  test("part 1") {
    assertEquals(part1(List(1L, 10L, 100L, 2024L)), 37327623L)
  }

  test("part 2") {
    assertEquals(part2(List(1L, 2L, 3L, 2024L)), 23)
  }
end MainSuite
