package fr.ggaly

import scala.annotation.targetName
import scala.collection.mutable

@main def main(): Unit =
  val maze = parseInput(Input.readLines())

  println(s"Part 1 result: ${part1(maze)}")

  println(s"Part 2 result: ${part2(maze)}")

def part1(maze: Maze): Int =
  // Dijkstra's algorithm
  val queue = mutable.PriorityQueue(Node(Position(maze.start, Right), 0))
  val visited = mutable.HashSet.empty[Position]

  var result = 0
  while result == 0 && queue.nonEmpty do
    val node = queue.dequeue()
    if node.position.coords == maze.end then result = node.cost
    else if visited.add(node.position) then
      node.moveForward(maze).foreach(queue.enqueue(_))
      queue.enqueue(node.turnLeft)
      queue.enqueue(node.turnRight)

  result
end part1

def part2(maze: Maze): Int = 0

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

final case class Node(position: Position, cost: Int) extends Ordered[Node]:
  override def compare(that: Node): Int = that.cost.compare(this.cost)
  def turnRight: Node = Node(position.turnRight, cost + 1000)
  def turnLeft: Node = Node(position.turnLeft, cost + 1000)
  def moveForward(maze: Maze): Option[Node] = position.moveForward(maze).map(Node(_, cost + 1))

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

final case class Weighted[T](value: T, var weight: Long) extends Ordered[Weighted[T]]:
  override def compare(that: Weighted[T]): Int = weight.compare(that.weight)
