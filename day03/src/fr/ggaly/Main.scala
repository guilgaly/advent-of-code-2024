package fr.ggaly

@main def main(): Unit =
  val input = Input.read

  println(s"Part 1 result: ${part1(input)}")

  println(s"Part 2 result: ${part2(input)}")

def part1(input: String): Int =
  parseInstructions(input).collect { case Mul(x, y) => x * y }.sum

def part2(input: String): Int =
  parseInstructions(input)
    .foldLeft((0, true)) { case ((acc, enabled), instr) =>
      instr match
        case Do                   => (acc, true)
        case Dont                 => (acc, false)
        case Mul(x, y) if enabled => (x * y + acc, true)
        case _                    => (acc, enabled)
    }
    ._1

sealed trait Instruction
case object Do extends Instruction
case object Dont extends Instruction
case class Mul(x: Int, y: Int) extends Instruction

private def parseInstructions(input: String): List[Instruction] =
  val regex = """do\(\)|don't\(\)|mul\(([0-9]+),([0-9]+)\)""".r
  regex
    .findAllIn(input)
    .matchData
    .map { r =>
      if r.matched == "do()" then Do
      else if r.matched == "don't()" then Dont
      else Mul(r.group(1).toInt, r.group(2).toInt)
    }
    .toList
end parseInstructions
