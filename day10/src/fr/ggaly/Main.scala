package fr.ggaly

@main def main(): Unit =
  val input = TrailMap(Input.readLines().map(_.map(_.asDigit).toArray).toArray)

  println(s"Part 1 result: ${part1(input)}")

  println(s"Part 2 result: ${part2(input)}")

def part1(trailMap: TrailMap): Int =
  def findTrailEnds(currentPoint: Point): Set[Point] =
    if trailMap.height(currentPoint) == 9 then Set(currentPoint)
    else trailMap.nextOnTrail(currentPoint).toSet.flatMap(findTrailEnds)
  end findTrailEnds

  trailMap.trailHeads.map(findTrailEnds(_).size).sum

def part2(trailMap: TrailMap): Int =
  def countTrails(currentPoint: Point): Int =
    if trailMap.height(currentPoint) == 9 then 1
    else trailMap.nextOnTrail(currentPoint).map(countTrails).sum
  end countTrails

  trailMap.trailHeads.map(countTrails).sum

final private case class Point(x: Int, y: Int)

final private case class TrailMap(map: Array[Array[Int]]):
  private val xMax: Int = map(0).length - 1
  private val yMax: Int = map.length - 1
  val trailHeads: List[Point] = map.iterator.zipWithIndex
    .flatMap((line, y) =>
      line.zipWithIndex.collect { case (0, x) => Point(x, y) },
    )
    .toList
  def height(p: Point): Int =
    map(p.y)(p.x)
  def heightOpt(p: Point): Option[Int] =
    val Point(x, y) = p
    if x >= 0 && x <= xMax && y >= 0 && y <= yMax then Some(height(p))
    else None
  def nextOnTrail(p: Point): List[Point] =
    val Point(x, y) = p
    Iterator(Point(x - 1, y), Point(x + 1, y), Point(x, y - 1), Point(x, y + 1))
      .filter(neighbour => heightOpt(neighbour).contains(height(p) + 1))
      .toList
end TrailMap
