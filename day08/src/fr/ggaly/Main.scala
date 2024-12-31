package fr.ggaly

import fr.ggaly.cartesiancoords.{BaseGrid, Coords}

import scala.annotation.tailrec

@main def main(): Unit =
  val (grid, antennas) = parseInput(Input.readLines())

  println(s"Part 1 result: ${part1(grid, antennas)}")

  println(s"Part 2 result: ${part2(grid, antennas)}")

def part1(grid: Grid, antennas: Map[Char, List[Coords]]): Int =
  def antennaPairAntinodes(a: Coords, b: Coords): List[Coords] =
    val vect = b - a
    List(b + vect, a - vect).filter(grid.contains)

  antinodes(antennas, antennaPairAntinodes)

def part2(grid: Grid, antennas: Map[Char, List[Coords]]): Int =
  def antennaPairAntinodes(a: Coords, b: Coords): Vector[Coords] =
    val vect = b - a

    @tailrec
    def positiveAntinodes(from: Coords, acc: Vector[Coords]): Vector[Coords] =
      val next = from + vect
      if grid.contains(next) then positiveAntinodes(next, acc :+ next) else acc

    @tailrec
    def negativeAntinodes(from: Coords, acc: Vector[Coords]): Vector[Coords] =
      val next = from - vect
      if grid.contains(next) then negativeAntinodes(next, acc :+ next) else acc

    positiveAntinodes(b, Vector.empty) ++ negativeAntinodes(a, Vector.empty)
  end antennaPairAntinodes

  antinodes(antennas, antennaPairAntinodes)
end part2

private def antinodes(
    antennas: Map[Char, List[Coords]],
    antennaPairAntinodes: (Coords, Coords) => Iterable[Coords],
) =
  antennas
    .flatMap { (_, points) =>
      points
        .combinations(2)
        .flatMap(combination => antennaPairAntinodes(combination.head, combination(1)))
    }
    .toSet
    .size

def parseInput(
    input: List[String],
): (Grid, Map[Char, List[Coords]]) =
  val height = input.size
  val width = input.head.length

  val antennas = input.zipWithIndex
    .flatMap((line, y) =>
      line.zipWithIndex.collect { case (c, x) if c != '.' => (c, Coords(x, y)) },
    )
    .groupMap(_._1)(_._2)

  (Grid(width, height), antennas)
end parseInput

final case class Grid(width: Int, height: Int) extends BaseGrid
