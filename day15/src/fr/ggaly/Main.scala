package fr.ggaly

import scala.annotation.targetName

@main def main(): Unit =
  val warehouseInput = Input.readLines("warehouse.txt")
  val movesInput = Input.read("moves.txt")

  println(s"Part 1 result: ${Part1(warehouseInput, movesInput)}")

  println(s"Part 2 result: ${Part2(warehouseInput, movesInput)}")

def parseMoves(input: String): List[Direction] = input.collect {
  case '^' => Up
  case '>' => Right
  case 'v' => Down
  case '<' => Left
}.toList

sealed trait Direction
case object Up extends Direction
case object Right extends Direction
case object Down extends Direction
case object Left extends Direction

final case class Coords(x: Long, y: Long):
  @targetName("add")
  def +(vector: Coords): Coords = Coords(x + vector.x, y + vector.y)
  @targetName("multiply")
  def *(mult: Long): Coords = Coords(x * mult, y * mult)
  def move(d: Direction): Coords = d match
    case Up    => copy(y = y - 1)
    case Right => copy(x = x + 1)
    case Down  => copy(y = y + 1)
    case Left  => copy(x = x - 1)
  def gps: Long = x + 100 * y
end Coords
