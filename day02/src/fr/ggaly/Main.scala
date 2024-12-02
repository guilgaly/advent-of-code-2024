package fr.ggaly

@main def main(): Unit =
  val input = Input.readLines
  val reports = input.map(line => line.split(' ').map(_.toInt).toList)

  println(s"Part 1 result: ${part1(reports)}")

  println(s"Part 2 result: ${part2()}")

def part1(reports: List[List[Int]]): Int =
  reports.count(report => safelyIncreasing(report) || safelyDecreasing(report))

def part2(): Int =
  0

private def safelyIncreasing(report: List[Int]): Boolean =
  report
    .sliding(2)
    .foldLeft(true) {
      case (false, _)                => false
      case (true, List(left, right)) => right > left && (right - left) <= 3
      case _                         => throw IllegalArgumentException()
    }

private def safelyDecreasing(report: List[Int]): Boolean =
  report
    .sliding(2)
    .foldLeft(true) {
      case (false, _)                => false
      case (true, List(left, right)) => left > right && (left - right) <= 3
      case _                         => throw IllegalArgumentException()
    }
