package fr.ggaly

import scala.annotation.targetName
import scala.collection.mutable

@main def main(): Unit =
  val maze = parseInput(Input.readLines())

  val (part1Result, part2Result) = part1and2(maze)

  println(s"Part 1 result: $part1Result")
  println(s"Part 2 result: $part2Result")

def part1and2(maze: Maze): (Int, Int) =
  // Dijkstra's algorithm + keeping track of alternative ways to get to the same visited position with the same cost
  val queue = mutable.PriorityQueue(Node(Position(maze.start, Right), 0, List.empty))
  val visited = mutable.HashMap.empty[Position, (Int, Set[Coords])]

  def enqueueNext(node: Node): Unit =
    node.moveForward(maze).foreach(queue.enqueue(_))
    queue.enqueue(node.turnLeft)
    queue.enqueue(node.turnRight)

  var bestCost = 0
  var bestPath = List.empty[Position]
  while bestCost == 0 && queue.nonEmpty do
    val node = queue.dequeue()
    if node.position.coords == maze.end then
      bestCost = node.cost
      bestPath = node.position :: node.walked

    visited.get(node.position) match
      case Some((cost, knownPaths)) if node.cost == cost =>
        visited.addOne(node.position, (cost, knownPaths ++ node.walked.map(_.coords)))
        enqueueNext(node)
      case None =>
        visited.addOne(node.position, (node.cost, node.walked.map(_.coords).toSet))
        enqueueNext(node)
      case _ =>
    end match
  end while

  val bestTiles = mutable.HashSet.empty[Coords]
  for tile <- bestPath do
    bestTiles.add(tile.coords)
    bestTiles.addAll(visited(tile)._2)

  (bestCost, bestTiles.size)
end part1and2

final case class Position(coords: Coords, direction: Direction):
  def turnRight: Position = direction match
    case Up    => copy(direction = Right)
    case Right => copy(direction = Down)
    case Down  => copy(direction = Left)
    case Left  => copy(direction = Up)

  def turnLeft: Position = direction match
    case Up    => copy(direction = Left)
    case Right => copy(direction = Up)
    case Down  => copy(direction = Right)
    case Left  => copy(direction = Down)

  def moveForward(maze: Maze): Option[Position] =
    val newCoords = coords.move(direction)
    if maze.walls.contains(newCoords) then None
    else Some(copy(coords = newCoords))
end Position

final case class Node(position: Position, cost: Int, walked: List[Position]) extends Ordered[Node]:
  override def compare(that: Node): Int = that.cost.compare(this.cost)
  def turnRight: Node = Node(position.turnRight, cost + 1000, position :: walked)
  def turnLeft: Node = Node(position.turnLeft, cost + 1000, position :: walked)
  def moveForward(maze: Maze): Option[Node] =
    position.moveForward(maze).map(Node(_, cost + 1, position :: walked))

def parseInput(input: List[String]): Maze =
  val tiles =
    input.zipWithIndex.flatMap((line, y) => line.zipWithIndex.map((c, x) => (c, Coords(x, y))))
  val start = tiles.collectFirst { case ('S', coords) => coords }.get
  val end = tiles.collectFirst { case ('E', coords) => coords }.get
  Maze(tiles.collect { case ('#', coords) => coords }.toSet, start, end)

final case class Maze(walls: Set[Coords], start: Coords, end: Coords)

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
