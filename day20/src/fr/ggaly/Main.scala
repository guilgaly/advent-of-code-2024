package fr.ggaly

import scala.annotation.{tailrec, targetName}
import Direction.*

@main def main(): Unit =
  val maze = parseInput(Input.readLines())

  println(s"Part 1 (alt) result: ${solve(maze, 2, 100)}")

  println(s"Part 2 result: ${solve(maze, 20, 100)}")

def solve(maze: Maze, cheatLength: Int, minTimeGained: Int): Int =
  val normalPath = findPath(maze)

  // Useful cheats are shortcuts to a later point on the normal path.
  var cheatsCount = 0
  for cheatAt <- 0 until (normalPath.size - minTimeGained) do
    val cheatStartPoint = normalPath(cheatAt)
    val restOfNormalPath = normalPath.drop(cheatAt + 1)
    restOfNormalPath.zipWithIndex.drop(minTimeGained).foreach { (p, idx) =>
      val distance = cheatStartPoint.manhattanDistance(p)
      if distance >= 2 && distance <= cheatLength && idx >= (100 + distance - 1) then
        cheatsCount += 1
    }
  end for

  cheatsCount
end solve

// There is only one linear path, no need for a search algorithm that can explore different paths
def findPath(maze: Maze): Vector[Coords] =
  @tailrec
  def recurs(path: Vector[Coords]): Vector[Coords] =
    if path.last == maze.end then path
    else
      maze
        .accessibleNeighbours(path.last)
        .find(c => path.size == 1 || c != path(path.size - 2)) match
        case Some(c) => recurs(path :+ c)
        case None    => throw IllegalStateException("Found a dead end (should not happen)")

  recurs(Vector(maze.start))
end findPath

def parseInput(input: List[String]): Maze =
  val width = input.head.length
  val height = input.size
  input.zipWithIndex
    .flatMap { case (line, y) => line.zipWithIndex.map { case (c, x) => (c, x, y) } }
    .foldLeft(Maze(Set.empty, width, height, Coords(0, 0), Coords(0, 0))) {
      case (acc, ('#', x, y)) => acc.copy(walls = acc.walls + Coords(x, y))
      case (acc, ('S', x, y)) => acc.copy(start = Coords(x, y))
      case (acc, ('E', x, y)) => acc.copy(end = Coords(x, y))
      case (acc, _)           => acc
    }
end parseInput

final case class Maze(walls: Set[Coords], width: Int, height: Int, start: Coords, end: Coords):
  def neighbours(c: Coords): List[Coords] = List(c + Up, c + Right, c + Down, c + Left)
    .filter { case Coords(x, y) => x >= 0 && x < width && y >= 0 && y < height }
  def accessibleNeighbours(c: Coords): List[Coords] = neighbours(c).filter(c => !walls.contains(c))
  def blockedNeighbours(c: Coords): List[Coords] = neighbours(c).filter(c => walls.contains(c))

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
  def manhattanDistance(other: Coords): Int = math.abs(other.x - x) + math.abs(other.y - y)
