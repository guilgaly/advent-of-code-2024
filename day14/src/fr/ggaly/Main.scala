package fr.ggaly

import java.io.PrintStream
import java.nio.file.{Files, Path, Paths}
import scala.util.Using

import fr.ggaly.cartesiancoords.{BaseGrid, Coords}

@main def main(): Unit =
  val input = parseInput(Input.readLines())

  val space = Space(101, 103)

  println(s"Part 1 result: ${part1(space, input)}")

  println(s"Part 2 result: ${part2(space, input)}")
end main

def part1(space: Space, input: List[Robot]): Int =
  val endState = (0 until 100).foldLeft(input) { (robots, i) =>
    robots.map { case Robot(position, speed) =>
      Robot(space.move(position, speed), speed)
    }
  }
  space.safetyFactor(endState)

def part2(space: Space, input: List[Robot]): Int =
  Files.createDirectories(Paths.get("day14/print1"))
  Files.createDirectories(Paths.get("day14/print2"))
  Files.createDirectories(Paths.get("day14/print3"))

  def next(robots: List[Robot]): List[Robot] =
    robots.map { case Robot(position, speed) =>
      Robot(space.move(position, speed), speed)
    }

  def printMap(dir: Path, robots: List[Robot], time: Int): Unit =
    Using(new PrintStream(Files.newOutputStream(dir.resolve(s"$time.txt")))) { ps =>
      space.printMap(robots, ps)
    }.get

  val dir1 = Paths.get("day14/print1")
  Files.createDirectories(dir1)
  (1 to 1_000).foldLeft(input) { (robots, i) =>
    val newRobots = next(robots)
    printMap(dir1, newRobots, i)
    newRobots
  }

  // Look through the printed files, notice two cycles where the outputs look non random; print only these cycles
  // When both cycles match, we get a christmas tree.

  val dir2 = Paths.get("day14/print2")
  Files.createDirectories(dir2)
  val dir3 = Paths.get("day14/print3")
  Files.createDirectories(dir3)
  val dir4 = Paths.get("day14/christmas_trees")
  Files.createDirectories(dir4)

  var response = 0
  (1 to 10_000).foldLeft(input) { (robots, i) =>
    val newRobots = next(robots)
    if (i - 4) % 103 == 0 then printMap(dir2, newRobots, i)
    if (i - 29) % 101 == 0 then printMap(dir3, newRobots, i)
    if (i - 4) % 103 == 0 && (i - 29) % 101 == 0 then
      printMap(dir4, newRobots, i)
      if response == 0 then response = i
    newRobots
  }

  response
end part2

def parseInput(lines: List[String]): List[Robot] =
  val regex = """p=(\d+),(\d+) v=(-?\d+),(-?\d+)""".r
  lines.map { line =>
    val matched = regex
      .findFirstMatchIn(line)
      .getOrElse(throw IllegalArgumentException(s"Unparsable line $line"))
    Robot(
      Coords(matched.group(1).toInt, matched.group(2).toInt),
      Coords(matched.group(3).toInt, matched.group(4).toInt),
    )
  }
end parseInput

final case class Robot(position: Coords, speed: Coords)

final case class Space(width: Int, height: Int) extends BaseGrid:
  private val xMid = width / 2
  private val yMid = height / 2

  def move(point: Coords, vector: Coords): Coords =
    val p = point + vector
    val x = if p.x >= width then p.x - width else if p.x < 0 then width + p.x else p.x
    val y = if p.y >= height then p.y - height else if p.y < 0 then height + p.y else p.y
    Coords(x, y)

  def safetyFactor(robots: List[Robot]): Int =
    val (tl, tr, bl, br) =
      robots.foldLeft((0, 0, 0, 0)) {
        case ((topLeft, topRight, bottomLeft, bottomRight), Robot(Coords(x, y), _)) =>
          if x < xMid && y < yMid then (topLeft + 1, topRight, bottomLeft, bottomRight)
          else if x > xMid && y < yMid then (topLeft, topRight + 1, bottomLeft, bottomRight)
          else if x < xMid && y > yMid then (topLeft, topRight, bottomLeft + 1, bottomRight)
          else if x > xMid && y > yMid then (topLeft, topRight, bottomLeft, bottomRight + 1)
          else (topLeft, topRight, bottomLeft, bottomRight)
      }
    tl * tr * bl * br
  end safetyFactor

  def printMap(robots: List[Robot], o: PrintStream): Unit =
    val robotsMap = robots.groupMapReduce(_.position)(_ => 1)(_ + _)
    for y <- 0 until height do
      for x <- 0 until width do
        robotsMap.get(Coords(x, y)) match
          case Some(count) => o.print(count)
          case None        => o.print(".")
      o.println()
  end printMap
end Space
