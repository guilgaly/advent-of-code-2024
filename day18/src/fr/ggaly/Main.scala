package fr.ggaly

import scala.annotation.{tailrec, targetName}
import scala.collection.mutable

@main def main(): Unit =
  val input = parseInput(Input.readLines())

  println(s"Part 1 result: ${part1(input, MemorySize(70, 70), 1024)}")

  println(s"Part 2 result: ${part2(input, MemorySize(70, 70))}")

def part1(input: List[Coords], memorySize: MemorySize, fallenBlocks: Int): Int =
  solve(input, memorySize, fallenBlocks).get

def part2(input: List[Coords], memorySize: MemorySize): String =
  @tailrec
  def countBlocks(input: List[Coords], memorySize: MemorySize, fallenBlocks: Int): Int =
    solve(input, memorySize, fallenBlocks) match
      case Some(_) => countBlocks(input, memorySize, fallenBlocks + 1)
      case None    => fallenBlocks

  val idx = countBlocks(input, memorySize, 0) - 1
  val blockedCoords = input(idx)
  s"${blockedCoords.x},${blockedCoords.y}"
end part2

private def solve(input: List[Coords], memorySize: MemorySize, fallenBlocks: Int): Option[Int] =
  val accessibleCoords = Array.fill(memorySize.height)(Array.fill(memorySize.width)(true))
  for Coords(x, y) <- input.take(fallenBlocks) do accessibleCoords(y)(x) = false

  val target = Coords(memorySize.xMax, memorySize.yMax)
  val visited = mutable.HashSet.empty[Coords]
  val queue = mutable.Queue((Coords(0, 0), 0)) // (coord, number of steps)
  var result: Option[Int] = None
  while result.isEmpty && queue.nonEmpty do
    val (coords, steps) = queue.dequeue()
    if coords == target then result = Some(steps)
    else if !visited.contains(coords) then
      visited.add(coords)
      memorySize
        .neighbours(coords)
        .filter(c => accessibleCoords(c.y)(c.x))
        .foreach(c => queue.enqueue((c, steps + 1)))
  end while

  result
end solve

def parseInput(input: List[String]): List[Coords] =
  input.map { line =>
    val Array(x, y) = line.split(',')
    Coords(x.toInt, y.toInt)
  }

object Direction:
  val Up: Coords = Coords(0, -1)
  val Right: Coords = Coords(1, 0)
  val Down: Coords = Coords(0, 1)
  val Left: Coords = Coords(-1, 0)

final case class Coords(x: Int, y: Int):
  @targetName("add")
  def +(vector: Coords): Coords = Coords(x + vector.x, y + vector.y)
  @targetName("multiply")
  def *(mult: Int): Coords = Coords(x * mult, y * mult)

final case class MemorySize(xMax: Int, yMax: Int):
  val width: Int = xMax + 1
  val height: Int = yMax + 1
  def neighbours(coords: Coords): List[Coords] =
    val Coords(x, y) = coords
    List(Coords(x, y - 1), Coords(x + 1, y), Coords(x, y + 1), Coords(x - 1, y))
      .filter { case Coords(x, y) => x >= 0 && x <= xMax && y >= 0 && y <= yMax }
