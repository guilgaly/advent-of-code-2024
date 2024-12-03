package fr.ggaly

@main def main(): Unit =
  val input = Input.readLines

  println(s"Part 1 result: ${part1(input.mkString("\n"))}")

  println(s"Part 2 result: ${part2()}")

def part1(input: String): Int =
  val regex = """mul\(([0-9]+),([0-9]+)\)""".r
  regex
    .findAllIn(input)
    .matchData
    .map(r => r.group(1).toInt * r.group(2).toInt)
    .sum

def part2(): Int =
  0
