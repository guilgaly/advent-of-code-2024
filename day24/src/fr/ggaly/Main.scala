package fr.ggaly

import scala.collection.mutable

@main def main(): Unit =
  val initialValues = parseInitialValues(Input.readLines("initialValues.txt"))
  val gates = parseGates(Input.readLines("gates.txt"))

  println(s"Part 1 result: ${part1(initialValues, gates)}")

  // Part 2 done by hand, see resources/part2.txt

def part1(initialValues: Map[String, Boolean], gates: List[Gate]): Long =
  val wireValues = initialValues.to(mutable.HashMap)

  // Loop until we aren't modifying any more wires (not the most elegant way, but it works)
  var wireCount = 0
  while wireCount != wireValues.size do
    wireCount = wireValues.size
    for gate <- gates do
      for
        in1 <- wireValues.get(gate.in1)
        in2 <- wireValues.get(gate.in2)
      do wireValues.addOne(gate.out -> gate(in1, in2))

  val binaryResult = wireValues
    .filter { case (k, _) => k.startsWith("z") }
    .toList
    .sortWith { case ((k1, _), (k2, _)) => k1 > k2 }
    .map { case (_, v) => if v then "1" else "0" }
    .mkString
  println(s"binaryResult: $binaryResult")
  java.lang.Long.parseLong(binaryResult, 2)
end part1

def parseInitialValues(input: List[String]): Map[String, Boolean] =
  input.map { line =>
    val split = line.split(':')
    split(0) -> (split(1).trim == "1")
  }.toMap

def parseGates(input: List[String]): List[Gate] =
  input.map { line =>
    val split = line.split(' ')
    val in1 = split(0)
    val in2 = split(2)
    val out = split(4)
    split(1) match
      case "AND" => And(in1, in2, out)
      case "OR"  => Or(in1, in2, out)
      case "XOR" => Xor(in1, in2, out)
      case _     => throw IllegalArgumentException(s"Cannot parse gate '$line'")
  }

sealed abstract class Gate:
  def in1: String
  def in2: String
  def out: String
  def apply(a: Boolean, b: Boolean): Boolean
final case class And(in1: String, in2: String, out: String) extends Gate:
  def apply(a: Boolean, b: Boolean): Boolean = a && b
final case class Or(in1: String, in2: String, out: String) extends Gate:
  def apply(a: Boolean, b: Boolean): Boolean = a || b
final case class Xor(in1: String, in2: String, out: String) extends Gate:
  def apply(a: Boolean, b: Boolean): Boolean = a != b
