package fr.ggaly

import scala.collection.mutable

@main def main(): Unit =
  val input = Farm(Input.readLines().map(_.toArray).toArray)

  println(s"Part 1 result: ${part1(input)}")

  println(s"Part 2 result: ${part2(input)}")

def part1(farm: Farm): Int =
  val regions = findRegions(farm)

  regions.map { region =>
    val area = region.plots.size
    val perimiter = region.plots.iterator
      .map(_.neighbours.count(neighbour => !farm.is(neighbour, region.plant)))
      .sum
    area * perimiter
  }.sum
end part1

def part2(farm: Farm): Int =
  val regions = findRegions(farm)

  regions.map { case Region(plant, plots) =>
    val minX = plots.map(_.x).min
    val maxX = plots.map(_.x).max
    val minY = plots.map(_.y).min
    val maxY = plots.map(_.y).max

    var fenceCount = 0

    // Horizontal fences
    for y <- minY to maxY do
      var topFence = false
      var bottomFence = false
      for x <- minX to maxX do
        val p = Coords(x, y)
        if topFence && (!plots.contains(p) || plots.contains(p.top)) then topFence = false
        else if !topFence && plots.contains(p) && !plots.contains(p.top) then
          topFence = true
          fenceCount += 1
        if bottomFence && (!plots.contains(p) || plots.contains(p.bottom)) then bottomFence = false
        else if !bottomFence && plots.contains(p) && !plots.contains(p.bottom) then
          bottomFence = true
          fenceCount += 1
      end for
    end for
    // Vertical fences
    for x <- minX to maxX do
      var leftFence = false
      var rightFence = false
      for y <- minY to maxY do
        val p = Coords(x, y)
        if leftFence && (!plots.contains(p) || plots.contains(p.left)) then leftFence = false
        else if !leftFence && plots.contains(p) && !plots.contains(p.left)
        then
          leftFence = true
          fenceCount += 1
        if rightFence && (!plots.contains(p) || plots.contains(p.right)) then rightFence = false
        else if !rightFence && plots.contains(p) && !plots.contains(p.right) then
          rightFence = true
          fenceCount += 1
      end for
    end for

    plots.size * fenceCount
  }.sum
end part2

private def findRegions(farm: Farm): List[Region] =
  val explored = mutable.Set.empty[Coords]
  var regions = List.empty[Region]

  for
    y <- 0 to farm.maxY
    x <- 0 to farm.maxX
    startingPos = Coords(x, y)
    if !explored.contains(startingPos)
  do
    val region = mutable.Set(startingPos)
    val plant = farm.get(startingPos)
    var lastExplored = Set(startingPos)
    while lastExplored.nonEmpty do
      lastExplored = lastExplored
        .flatMap(_.neighbours)
        .filter(p => farm.is(p, plant) && !region.contains(p))
      region.addAll(lastExplored)
    explored.addAll(region)

    regions = Region(plant, region.toSet) :: regions
  end for

  regions
end findRegions

final case class Coords(x: Int, y: Int):
  def neighbours: Set[Coords] = Set(left, top, right, bottom)
  def left: Coords = Coords(x - 1, y)
  def right: Coords = Coords(x + 1, y)
  def top: Coords = Coords(x, y - 1)
  def bottom: Coords = Coords(x, y + 1)

final case class Farm(plots: Array[Array[Char]]):
  val maxX: Int = plots(0).length - 1
  val maxY: Int = plots.length - 1
  def get(pos: Coords): Char =
    plots(pos.y)(pos.x)
  def exists(pos: Coords): Boolean =
    pos.x >= 0 && pos.x <= maxX && pos.y >= 0 && pos.y <= maxY
  def getOpt(pos: Coords): Option[Char] =
    if exists(pos) then Some(get(pos)) else None
  def is(pos: Coords, plant: Char): Boolean =
    getOpt(pos).contains(plant)
end Farm

final case class Region(plant: Char, plots: Set[Coords])
