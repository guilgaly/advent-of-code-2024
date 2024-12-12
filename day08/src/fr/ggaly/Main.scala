package fr.ggaly

import scala.collection.mutable

@main def main(): Unit =
  val input = Input.readLines()

  println(s"Part 1 result: ${part1(input)}")

  println(s"Part 2 result: ${part2(input)}")

def part1(input: List[String]): Int =
  val (xMax, yMax, antennas) = parseInput(input)

  def antennaPairAntinodes(a: Point, b: Point): List[Point] =
    // Vector from a to b
    val vect = Point(b.x - a.x, b.y - a.y)
    List(Point(b.x + vect.x, b.y + vect.y), Point(a.x - vect.x, a.y - vect.y))
      .filter(_.isValid(xMax, yMax))

  antinodes(antennas, antennaPairAntinodes)
end part1

def part2(input: List[String]): Int =
  val (xMax, yMax, antennas) = parseInput(input)

  def antennaPairAntinodes(a: Point, b: Point): List[Point] =
    // Vector from a to b
    val vect = Point(b.x - a.x, b.y - a.y)

    val buffer = mutable.Buffer(a, b)

    var next = Point(b.x + vect.x, b.y + vect.y)
    while next.isValid(xMax, yMax) do
      buffer.append(next)
      next = Point(next.x + vect.x, next.y + vect.y)

    next = Point(a.x - vect.x, a.y - vect.y)
    while next.isValid(xMax, yMax) do
      buffer.append(next)
      next = Point(next.x - vect.x, next.y - vect.y)

    buffer.toList
  end antennaPairAntinodes

  antinodes(antennas, antennaPairAntinodes)
end part2

final case class Point(x: Int, y: Int):
  def isValid(xMax: Int, yMax: Int): Boolean =
    x >= 0 && x <= xMax && y >= 0 && y <= yMax

private def parseInput(
    input: List[String],
): (Int, Int, Map[Char, List[Point]]) =
  val yMax = input.size - 1
  val xMax = input.head.length - 1

  val antennas = input.zipWithIndex
    .flatMap((line, y) => line.zipWithIndex.collect { case (c, x) if c != '.' => (c, Point(x, y)) })
    .groupMap(_._1)(_._2)

  (xMax, yMax, antennas)
end parseInput

private def antinodes(
    antennas: Map[Char, List[Point]],
    antennaPairAntinodes: (Point, Point) => List[Point],
) =
  antennas
    .flatMap { (_, points) =>
      points
        .combinations(2)
        .flatMap(combination => antennaPairAntinodes(combination.head, combination(1)))
    }
    .toSet
    .size
