package fr.ggaly

import fr.ggaly.cartesiancoords.{BaseGrid, Coords}

@main def main(): Unit =
  val input = parseInput(Input.readLines())

  println(s"Part 1 result: ${part1(input)}")

  println(s"Part 2 result: ${part2(input)}")

def part1(trailMap: TrailMap): Int =
  def findTrailEnds(currentPoint: Coords): Set[Coords] =
    if trailMap.height(currentPoint) == 9 then Set(currentPoint)
    else trailMap.nextOnTrail(currentPoint).toSet.flatMap(findTrailEnds)
  end findTrailEnds

  trailMap.trailHeads.map(findTrailEnds(_).size).sum

def part2(trailMap: TrailMap): Int =
  def countTrails(currentPoint: Coords): Int =
    if trailMap.height(currentPoint) == 9 then 1
    else trailMap.nextOnTrail(currentPoint).map(countTrails).sum
  end countTrails

  trailMap.trailHeads.map(countTrails).sum

final private case class TrailMap(map: Array[Array[Int]]) extends BaseGrid:
  val width: Int = map(0).length
  val height: Int = map.length

  val trailHeads: List[Coords] = map.iterator.zipWithIndex
    .flatMap((line, y) => line.zipWithIndex.collect { case (0, x) => Coords(x, y) })
    .toList

  def height(p: Coords): Int =
    map(p.y)(p.x)

  def nextOnTrail(p: Coords): List[Coords] =
    val currHeight = height(p)
    accessibleNeighbours(p).filter(height(_) == currHeight + 1).toList
end TrailMap

def parseInput(input: List[String]): TrailMap =
  TrailMap(input.map(_.map(_.asDigit).toArray).toArray)
