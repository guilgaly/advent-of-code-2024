package fr.ggaly

import scala.collection.mutable

@main def main(): Unit =
  val input = Input.readLines()

  println(s"Part 1 result: ${part1(input)}")

  println(s"Part 2 result: ${part2(input)}")

def part1(input: List[String]): Int =
  val maxY = input.size - 1
  val maxX = input.head.length - 1
  val startY = input.indexWhere(_.contains('^'))
  val startX = input(startY).indexOf('^')
  val map =
    input.map(_.map(c => c != '#').toVector).toVector // 'true' == accessible

  var x = startX
  var y = startY
  var orientation: Orientation = Up
  val visited = mutable.HashSet.empty[(Int, Int)]
  while x >= 0 && x <= maxX && y >= 0 && y <= maxY do
    visited.add((x, y))
    orientation match
      case Up =>
        if y == 0 || map(y - 1)(x) then y = y - 1 else orientation = Right
      case Right =>
        if x == maxX || map(y)(x + 1) then x = x + 1 else orientation = Down
      case Down =>
        if y == maxY || map(y + 1)(x) then y = y + 1 else orientation = Left
      case Left =>
        if x == 0 || map(y)(x - 1) then x = x - 1 else orientation = Up
    end match
  end while
  visited.size
end part1

def part2(input: List[String]): Int =
  val maxY = input.size - 1
  val maxX = input.head.length - 1
  val startY = input.indexWhere(_.contains('^'))
  val startX = input(startY).indexOf('^')
  val map =
    input.map(_.map(c => c != '#').toVector).toVector // 'true' == accessible

  val possibleObstacles = for
    x <- 0 to maxX
    y <- 0 to maxY
    // add obstacle in an empty space and not the starting position
    if (x != startX || y != startY) && map(y)(x)
  yield (x, y)

  val validObstacles = possibleObstacles.filter { case (obstacleX, obstacleY) =>
    def free(x: Int, y: Int) = (x != obstacleX || y != obstacleY) && map(y)(x)

    var x = startX
    var y = startY
    var orientation: Orientation = Up
    val visited = mutable.HashSet.empty[(Int, Int, Orientation)]
    var looped = false
    while !looped && x >= 0 && x <= maxX && y >= 0 && y <= maxY do
      visited.add((x, y, orientation))
      orientation match
        case Up =>
          if y == 0 || free(x, y - 1) then y = y - 1 else orientation = Right
        case Right =>
          if x == maxX || free(x + 1, y) then x = x + 1 else orientation = Down
        case Down =>
          if y == maxY || free(x, y + 1) then y = y + 1 else orientation = Left
        case Left =>
          if x == 0 || free(x - 1, y) then x = x - 1 else orientation = Up
      end match
      if visited.contains((x, y, orientation)) then looped = true
    end while
    looped
  }

  validObstacles.size
end part2

sealed trait Orientation
case object Up extends Orientation
case object Right extends Orientation
case object Down extends Orientation
case object Left extends Orientation
