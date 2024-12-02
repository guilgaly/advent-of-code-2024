package fr.ggaly

@main def main(): Unit =
  val input = Input.readLines
  val (list1, list2) = input.map { line =>
    val split = line.split("\\s+").map(_.toInt)
    (split(0), split(1))
  }.unzip

  println(s"Part 1 result: ${part1(list1, list2)}")

  println(s"Part 2 result: ${part2(list1, list2)}")
end main

def part1(list1: List[Int], list2: List[Int]): Int =
  val sorted1 = list1.sorted
  val sorted2 = list2.sorted
  sorted1.zip(sorted2).map { case (id1, id2) => (id2 - id1).abs }.sum

def part2(list1: List[Int], list2: List[Int]): Int =
  list1.map { left => left * list2.count(_ == left) }.sum
