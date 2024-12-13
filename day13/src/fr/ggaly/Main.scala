package fr.ggaly

import scala.annotation.targetName

@main def main(): Unit =

  val input = parseClawMachines(Input.readLines())

  println(s"Part 1 result: ${part1(input)}")

  println(s"Part 2 result: ${part2(input)}")

def part1(input: List[ClawMachine]): Int =
  input.flatMap { case ClawMachine(a, b, prize) =>
    (for
      aPresses <- 0 to 100
      bPresses <- 0 to 100
      if Coords(0, 0) + (a * aPresses) + (b * bPresses) == prize
    yield aPresses * 3 + bPresses).minOption
  }.sum

def part2(input: List[ClawMachine]): Long =
  val actualInput =
    input.map(m => m.copy(prize = m.prize + Coords(10000000000000L, 10000000000000L)))

  actualInput.map { case ClawMachine(a, b, prize) =>
    // system of two linear equations in two variables
    // a.x * aPresses + b.x * bPresses = prize.x
    // a.y * aPresses + b.y * bPresses = prize.y
    // This solution works because, in our acse, there is only zero or one solution;
    // otherwise, determinant == 0 could also mean there are infinitely many solutions.

    val determinant = a.x * b.y - a.y * b.x
    if determinant != 0L then
      val aPresses = (b.y * prize.x - b.x * prize.y) / determinant
      val aPressesMod = (b.y * prize.x - b.x * prize.y) % determinant
      val bPresses = (-a.y * prize.x + a.x * prize.y) / determinant
      val bPressesMod = (-a.y * prize.x + a.x * prize.y) % determinant
      if aPresses > 0L && aPressesMod == 0 && bPresses > 0L && bPressesMod == 0 then
        aPresses * 3L + bPresses
      else 0L
    else 0L
    end if
  }.sum
end part2

private def parseClawMachines(input: List[String]): List[ClawMachine] =
  val aButtonRegex = """Button A: X\+(\d+), Y\+(\d+)""".r
  val bButtonRegex = """Button B: X\+(\d+), Y\+(\d+)""".r
  val prizeRegex = """Prize: X=(\d+), Y=(\d+)""".r
  input
    .sliding(3, 4)
    .map { lines =>
      val matchA = aButtonRegex.findFirstMatchIn(lines.head).get
      val a = Coords(matchA.group(1).toLong, matchA.group(2).toLong)
      val matchB = bButtonRegex.findFirstMatchIn(lines(1)).get
      val b = Coords(matchB.group(1).toLong, matchB.group(2).toLong)
      val matchPrize = prizeRegex.findFirstMatchIn(lines(2)).get
      val prize = Coords(matchPrize.group(1).toLong, matchPrize.group(2).toLong)
      ClawMachine(a, b, prize)
    }
    .toList
end parseClawMachines

final case class Coords(x: Long, y: Long):
  @targetName("plus")
  def +(vector: Coords): Coords = Coords(x + vector.x, y + vector.y)
  @targetName("times")
  def *(mult: Long): Coords = Coords(x * mult, y * mult)

final case class ClawMachine(a: Coords, b: Coords, prize: Coords)
