package fr.ggaly

import fr.ggaly.cartesiancoords.{BaseGrid, Coords}

import scala.annotation.tailrec
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

  val target = Coords(memorySize.width - 1, memorySize.height - 1)
  val visited = mutable.HashSet.empty[Coords]
  val queue = mutable.Queue((Coords(0, 0), 0)) // (coord, number of steps)
  var result: Option[Int] = None
  while result.isEmpty && queue.nonEmpty do
    val (coords, steps) = queue.dequeue()
    if coords == target then result = Some(steps)
    else if !visited.contains(coords) then
      visited.add(coords)
      memorySize
        .accessibleNeighbours(coords)
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

object MemorySize:
  def apply(xMax: Int, yMax: Int): MemorySize = new MemorySize(xMax + 1, yMax + 1)

final case class MemorySize(width: Int, height: Int) extends BaseGrid
