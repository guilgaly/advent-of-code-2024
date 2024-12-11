package fr.ggaly

@main def main(): Unit =
  val input = List(1117L, 0L, 8L, 21078L, 2389032L, 142881L, 93L, 385L)

  println(s"Part 1 result: ${part1(input)}")

  println(s"Part 2 result: ${part2(input)}")

def part1(input: List[Long]): Int =
  (0 until 25)
    .foldLeft(input)((acc, i) => acc.flatMap(nextStep))
    .size

def part2(input: List[Long]): Long =
  // Map (stone value -> number of stones of this value)
  val initStoneCounts = input.groupMapReduce(identity)(_ => 1L)(_ + _)

  (0 until 75)
    .foldLeft(initStoneCounts) { (prev, _) =>
      prev.toList
        .flatMap((stone, count) => nextStep(stone).map(value => (value, count)))
        .groupMapReduce(_._1)(_._2)(_ + _)
    }
    .values
    .sum
end part2

private def nextStep(stone: Long): List[Long] = stone match
  case 0                  => List(1)
  case Split(left, right) => List(left, right)
  case other              => List(other * 2024)

object Split:
  def unapply(n: Long): Option[(Long, Long)] =
    val strN = n.toString
    if strN.length % 2 == 0 then
      val (left, right) = strN.splitAt(strN.length / 2)
      Some((left.toLong, right.toLong))
    else None
