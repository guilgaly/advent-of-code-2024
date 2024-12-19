package fr.ggaly

import scala.collection.mutable

@main def main(): Unit =
  val towels = Input.read("towels.txt").split(',').map(_.trim).toList
  val designs = Input.readLines("designs.txt")

  println(s"Part 1 result: ${part1(towels, designs)}")

  println(s"Part 2 result: ${part2(towels, designs)}")

def part1(towels: List[String], designs: List[String]): Int =
  def isValid(design: String): Boolean =
    design.isEmpty || towels.exists(t => design.startsWith(t) && isValid(design.drop(t.length)))

  designs.count(isValid)

def part2(towels: List[String], designs: List[String]): Long =
  val cache = mutable.HashMap.empty[String, Long]

  def countArrangements(design: String): Long =
    if design.isEmpty then 1L
    else
      cache.get(design) match
        case Some(count) => count
        case _ =>
          val count = towels.collect {
            case t if design.startsWith(t) => countArrangements(design.drop(t.length))
          }.sum
          cache.addOne(design -> count)
          count

  designs.map(countArrangements).sum
end part2
