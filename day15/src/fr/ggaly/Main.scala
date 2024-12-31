package fr.ggaly

import fr.ggaly.cartesiancoords.{Coords, Direction}
import fr.ggaly.cartesiancoords.Direction.{Down, Left, Right, Up}

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

def gps(c: Coords): Int = c.x + 100 * c.y
