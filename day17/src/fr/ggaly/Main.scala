package fr.ggaly

import scala.annotation.tailrec

@main def main(): Unit =
  val initialState =
    Computer(27575648, 0, 0, Array(2, 4, 1, 2, 7, 5, 4, 1, 1, 3, 5, 5, 0, 3, 3, 0))

  println(s"Part 1 result: ${part1(initialState)}")

  println(s"Part 2 result: ${part2(initialState)}")

def part1(initialState: Computer): String =
  initialState.run.readOutput.mkString(",")

def part2(initialState: Computer): Long =
  // finding 3 bits digits one after the other
  def findA(remainingDigits: Int, acc: Long): Option[Long] =
    (0 until 8).flatMap { candidate =>
      val current = acc * 8 + candidate
      val computer = initialState.copy(registers = initialState.registers.copy(a = current))
      if computer.run.readOutput == computer.program.drop(remainingDigits).toList then
        if remainingDigits == 0 then Some(current)
        else findA(remainingDigits - 1, current)
      else None
    }.headOption
  end findA

  findA(initialState.program.length - 1, 0L).get
end part2

object Computer:
  def apply(a: Long, b: Long, c: Long, program: Array[Int]): Computer =
    Computer(Registers(a, b, c), program.map(_.toLong), 0, Nil)

final case class Computer(
    registers: Registers,
    program: Array[Long],
    pointer: Int,
    output: List[Long],
):
  def readOutput: List[Long] = output.reverse

  def run: Computer =
    @tailrec
    def recurs(current: Computer): Computer = current.stepForward match
      case Some(next) => recurs(next)
      case None       => current
    recurs(this)

  private def stepForward: Option[Computer] =
    if pointer >= program.length then None
    else
      Some(
        Instruction.fromOpcode(program(pointer)) match
          case Adv =>
            copy(registers = registers.copy(a = divisionRes), pointer = pointer + 2)
          case Bxl =>
            val res = registers.b ^ litteralOperand
            copy(registers = registers.copy(b = res), pointer = pointer + 2)
          case Bst =>
            val res = comboOperand % 8
            copy(registers = registers.copy(b = res), pointer = pointer + 2)
          case Jnz =>
            if registers.a == 0 then copy(pointer = pointer + 2)
            else copy(pointer = litteralOperand.toInt)
          case Bxc =>
            val res = registers.b ^ registers.c
            copy(registers = registers.copy(b = res), pointer = pointer + 2)
          case Out =>
            val res = comboOperand % 8
            copy(output = res :: output, pointer = pointer + 2)
          case Bdv =>
            copy(registers = registers.copy(b = divisionRes), pointer = pointer + 2)
          case Cdv =>
            copy(registers = registers.copy(c = divisionRes), pointer = pointer + 2),
      )

  private def divisionRes = registers.a / BigInt(2).pow(comboOperand.toInt).toInt

  private def litteralOperand: Long = program(pointer + 1)

  private def comboOperand: Long =
    program(pointer + 1) match
      case 4                                          => registers.a
      case 5                                          => registers.b
      case 6                                          => registers.c
      case litteral if 0 <= litteral && litteral <= 3 => litteral
      case other => throw IllegalArgumentException(s"Illegal combo operand $other")
end Computer

final case class Registers(a: Long, b: Long, c: Long)

object Instruction:
  def fromOpcode(opcode: Long): Instruction = opcode match
    case 0 => Adv
    case 1 => Bxl
    case 2 => Bst
    case 3 => Jnz
    case 4 => Bxc
    case 5 => Out
    case 6 => Bdv
    case 7 => Cdv
    case _ => throw IllegalArgumentException(s"Illegal opcode $opcode")
end Instruction

sealed trait Instruction
case object Adv extends Instruction
case object Bxl extends Instruction
case object Bst extends Instruction
case object Jnz extends Instruction
case object Bxc extends Instruction
case object Out extends Instruction
case object Bdv extends Instruction
case object Cdv extends Instruction
