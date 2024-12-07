package fr.ggaly

@main def main(): Unit =
  val input = Input.readLines().map { line =>
    val s = line.split(": ")
    val testValue = s(0).toLong
    val numbers = s(1).split(' ').map(_.toLong).toList
    Equation(testValue, numbers)
  }

  println(s"Part 1 result: ${part1(input)}")

  println(s"Part 2 result: ${part2(input)}")
end main

case class Equation(testValue: Long, numbers: List[Long])

def part1(input: List[Equation]): Long =
  calibration(input, (a, b) => List(a + b, a * b))

def part2(input: List[Equation]): Long =
  calibration(input, (a, b) => List(a + b, a * b, concat(a, b)))

private def calibration(
    input: List[Equation],
    operations: (a: Long, b: Long) => List[Long],
) =
  input
    .filter {
      case Equation(testValue, first :: rest) =>
        rest
          .foldLeft(List(first))((acc, next) =>
            acc.flatMap(a => operations(a, next)).filter(_ <= testValue),
          )
          .contains(testValue)
      case _ =>
        throw new IllegalStateException()
    }
    .map(_.testValue)
    .sum

private def concat(a: Long, b: Long): Long =
  (a.toString + b.toString).toLong
