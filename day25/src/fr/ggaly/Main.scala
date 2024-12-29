package fr.ggaly

import scala.collection.mutable

@main def main(): Unit =
  val (locks, keys) = parseInput(Input.read())

  println(s"Part 1 result: ${part1(locks, keys)}")

  println(s"Part 2 result: ${part2()}")

def part1(locks: List[Array[Int]], keys: List[Array[Int]]): Int =
  def fits(lock: Array[Int], key: Array[Int]): Boolean =
    (0 to 4).forall(i => lock(i) + key(i) <= 5)

  locks.foldLeft(0)((acc, lock) => acc + keys.count(key => fits(lock, key)))

def part2(): Int =
  0

/**
 * @return
 *   (locks, keys)
 */
def parseInput(input: String): (List[Array[Int]], List[Array[Int]]) =
  val locks = mutable.Buffer.empty[Array[Int]]
  val keys = mutable.Buffer.empty[Array[Int]]

  input.split("\\n\\n").foreach { schematic =>
    val lines = schematic.linesIterator.toList
    if lines.head == "#####" then
      // lock
      val lock = new Array[Int](5)
      for i <- 0 to 4 do
        val length = (1 to 6).find(j => lines(j)(i) == '.').get - 1
        lock(i) = length
      locks.append(lock)
    else
      // key
      val key = new Array[Int](5)
      for i <- 0 to 4 do
        val length = 5 - (5 to 0 by -1).find(j => lines(j)(i) == '.').get
        key(i) = length
      keys.append(key)
    end if
  }

  (locks.toList, keys.toList)
end parseInput
