package fr.ggaly

import scala.collection.mutable
import fr.ggaly.cartesiancoords.{BaseGrid, Coords, Direction}
import fr.ggaly.cartesiancoords.Direction.Right

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
  def turnRight: Position = copy(direction = direction.turnRight)

  def turnLeft: Position = copy(direction = direction.turnLeft)

  def moveForward(maze: Maze): Option[Position] =
    val newCoords = coords + direction
    if maze.blocked.contains(newCoords) then None
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

final case class Maze(override val blocked: Set[Coords], start: Coords, end: Coords)
    extends BaseGrid:
  override val width: Int = blocked.map(_.x).max + 1
  override val height: Int = blocked.map(_.y).max + 1

def gps(p: Coords): Int = p.x + 100 * p.y
