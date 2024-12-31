package fr.ggaly

import fr.ggaly.cartesiancoords.Coords

import scala.annotation.{tailrec, targetName}
import scala.collection.mutable

@main def main(): Unit =
  val codes = parseInput(
    List(
      "382A",
      "463A",
      "935A",
      "279A",
      "480A",
    ),
  )

  println(s"Part 1 result: ${part1(codes)}")

  println(s"Part 2 result: ${part2(codes)}")
end main

def part1(inputs: List[List[Key]]): Int =
  inputs.map { input =>
    val numPart = numericPart(input)

    val possibilities =
      for
        dial1 <- NumericKeypad.dial(input)
        dial2 <- DirectionalKeypad.dial(dial1)
        dial3 <- DirectionalKeypad.dial(dial2)
      yield dial3

    numericPart(input) * possibilities.minBy(_.size).size
  }.sum

def part2(inputs: List[List[Key]]): Long =
  val cache = mutable.HashMap.empty[(Key, Key, Int), Long]

  def dialListCount(list: List[Key], depth: Int): Long =
    if depth == 0 then list.size
    else
      list
        .foldLeft((AKey: Key, 0L)) { case ((previous, count), next) =>
          (next, count + dialSingleCount(previous, next, depth))
        }
        ._2

  def dialSingleCount(from: Key, to: Key, depth: Int): Long =
    cache.get(from, to, depth) match
      case Some(value) => value
      case None =>
        val result = DirectionalKeypad
          .dialOneMove(from, to)
          .map(path => dialListCount(path, depth - 1))
          .min
        cache.addOne((from, to, depth) -> result)
        result

  inputs.map { input =>
    val numPart = numericPart(input)
    val possibilities = NumericKeypad.dial(input).map(dial1 => dialListCount(dial1, 25))
    numericPart(input) * possibilities.min
  }.sum
end part2

def numericPart(code: List[Key]): Int =
  code.collect { case NumberKey(digit) => digit }.mkString.toInt

trait Keypad:
  protected val keyCoordinates: Map[Key, Coords]
  protected val forbidden: Coords

  def dial(input: List[Key]): List[List[Key]] =
    @tailrec
    def recurs(keys: List[Key], accPaths: List[List[Key]]): List[List[Key]] =
      keys match
        case prev :: next :: tail =>
          val newPaths = dialOneMove(prev, next)
          val newAccPaths =
            for
              newPath <- newPaths
              existingPath <- accPaths
            yield existingPath ++ newPath
          recurs(keys.tail, newAccPaths)
        case _ => accPaths

    recurs(AKey :: input, List(List.empty)).distinct
  end dial

  def dialOneMove(from: Key, to: Key): List[List[Key]] =
    val coordsFrom = keyCoordinates(from)
    val coordsTo = keyCoordinates(to)
    val vector = coordsTo - coordsFrom
    val leftRightKeyPresses =
      if vector.x >= 0 then List.fill(vector.x)(RightKey) else List.fill(-vector.x)(LeftKey)
    val upDownKeyPresses =
      if vector.y >= 0 then List.fill(vector.y)(DownKey) else List.fill(-vector.y)(UpKey)
    // It's always more efficient to do all the vertical or all the horizontal moves first
    // (don't alternate between the two)
    List(
      leftRightKeyPresses ++ upDownKeyPresses :+ AKey,
      upDownKeyPresses ++ leftRightKeyPresses :+ AKey,
    )
      .filter(p => isPathValid(coordsFrom, p))
      .distinct
  end dialOneMove

  @tailrec
  private def isPathValid(from: Coords, path: List[Key]): Boolean =
    def keyToMove(key: Key): Coords = key match
      case k: DirectionKey => k.move
      case _               => Coords(0, 0)

    if from == forbidden then false
    else if path.isEmpty then true
    else
      val newPosition = path.head match
        case k: DirectionKey => from + k.move
        case _               => from
      isPathValid(newPosition, path.tail)
  end isPathValid
end Keypad

object NumericKeypad extends Keypad:
  override protected val keyCoordinates: Map[Key, Coords] = Map(
    NumberKey(7) -> Coords(0, 0),
    NumberKey(8) -> Coords(1, 0),
    NumberKey(9) -> Coords(2, 0),
    NumberKey(4) -> Coords(0, 1),
    NumberKey(5) -> Coords(1, 1),
    NumberKey(6) -> Coords(2, 1),
    NumberKey(1) -> Coords(0, 2),
    NumberKey(2) -> Coords(1, 2),
    NumberKey(3) -> Coords(2, 2),
    NumberKey(0) -> Coords(1, 3),
    AKey -> Coords(2, 3),
  )
  override protected val forbidden: Coords = Coords(0, 3)
end NumericKeypad

object DirectionalKeypad extends Keypad:
  override protected val keyCoordinates: Map[Key, Coords] = Map(
    UpKey -> Coords(1, 0),
    AKey -> Coords(2, 0),
    LeftKey -> Coords(0, 1),
    DownKey -> Coords(1, 1),
    RightKey -> Coords(2, 1),
  )
  override protected val forbidden: Coords = Coords(0, 0)
end DirectionalKeypad

sealed abstract class Key(val char: Char):
  override def toString: String = char.toString
sealed abstract class DirectionKey(char: Char, var move: Coords) extends Key(char)
case object UpKey extends DirectionKey('^', Coords(0, -1))
case object RightKey extends DirectionKey('>', Coords(1, 0))
case object DownKey extends DirectionKey('v', Coords(0, 1))
case object LeftKey extends DirectionKey('<', Coords(-1, 0))
case object AKey extends Key('A')
final case class NumberKey(value: Int) extends Key(value.toString.head)

private def parseInput(input: List[String]): List[List[Key]] =
  input.map(_.map {
    case 'A'            => AKey
    case d if d.isDigit => NumberKey(d.asDigit)
    case other          => throw IllegalArgumentException(s"Cannot parse key $other")
  }.toList)
