package fr.ggaly.cartesiancoords

import scala.annotation.targetName

//object Coords:
//  val Up: Coords = Coords(0, -1)
//  val UpRight: Coords = Coords(1, -1)
//  val Right: Coords = Coords(1, 0)
//  val DownRight: Coords = Coords(1, 1)
//  val Down: Coords = Coords(0, 1)
//  val DownLeft: Coords = Coords(-1, 1)
//  val Left: Coords = Coords(-1, 0)
//  val UpLeft: Coords = Coords(-1, -1)
//  def orthDirections: Iterator[Coords] = Iterator(Up, Right, Down, Left)
//  def allDirections: Iterator[Coords] =
//    Iterator(Up, UpRight, Right, DownRight, Down, DownLeft, Left, UpLeft)
//end Coords

trait CoordsLike:
  def x: Int
  def y: Int

/**
 * Represents either a vector or a point (i.e. a vector from the origin point) in cartesian
 * coordinates.
 *
 * @param x
 *   horizontal coordinate, positive towards the right
 * @param y
 *   vertical coordinates, positive towards the bottom.
 */
final case class Coords(x: Int, y: Int) extends CoordsLike:
  import Coords.*

  @targetName("add")
  def +(other: CoordsLike): Coords = Coords(x + other.x, y + other.y)

  @targetName("substract")
  def -(other: CoordsLike): Coords = Coords(x - other.x, y - other.y)

  @targetName("multiply")
  def *(mult: Int): Coords = Coords(x * mult, y * mult)

  def manhattanDistance(other: Coords): Int = math.abs(other.x - x) + math.abs(other.y - y)

  /** Orthogonal neighbours (not including diagonals). */
  def neighbours: Iterator[Coords] =
    Direction.values.iterator.map(this + _)
end Coords
