package fr.ggaly

import scala.annotation.tailrec
import scala.collection.mutable

@main def main(): Unit =
  val rules = Input
    .readLines("rules.txt")
    .map(_.split('|').map(_.toInt) match
      case Array(left, right) => (left, right),
    )
  val updates =
    Input.readLines("updates.txt").map(_.split(',').map(_.toInt).toList)

  println(s"Part 1 result: ${part1(rules, updates)}")

  println(s"Part 2 result: ${part2(rules, updates)}")
end main

def part1(rules: List[(Int, Int)], updates: List[List[Int]]): Int =
  updates
    .filter(isUpdateValid(rules))
    .map(update => update(update.length / 2))
    .sum

def part2(rules: List[(Int, Int)], updates: List[List[Int]]): Int =
  val isValid = isUpdateValid(rules)

  @tailrec
  def recursiveReOrder(update: List[Int]): List[Int] =
    if isValid(update) then update
    else
      val reordered = update.to(mutable.ListBuffer)
      for case (left, right) <- rules do
        val leftIndex = reordered.indexOf(left)
        val rightIndex = reordered.indexOf(right)
        if leftIndex >= 0 && rightIndex >= 0 && rightIndex < leftIndex then
          val removed = reordered.remove(leftIndex)
          reordered.insert(rightIndex, removed)
      // Keep applying until all rules are satisfied together
      recursiveReOrder(reordered.toList)
    end if
  end recursiveReOrder

  updates
    .filter(!isValid(_))
    .map { update =>
      val reordered = recursiveReOrder(update)
      reordered(reordered.length / 2)
    }
    .sum
end part2

private def isUpdateValid(rules: List[(Int, Int)])(update: List[Int]): Boolean =
  rules.forall { case (left, right) =>
    val leftIndex = update.indexOf(left)
    val rightIndex = update.indexOf(right)
    leftIndex < 0 || rightIndex < 0 || leftIndex < rightIndex
  }
