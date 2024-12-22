package fr.ggaly

import scala.collection.mutable

@main def main(): Unit =
  val input = Input.readLines().map(_.toLong)

  println(s"Part 1 result: ${part1(input)}")

  println(s"Part 2 result: ${part2(input)}")

def part1(input: List[Long]): Long =
  input.map(initSecret => (1 to 2000).foldLeft(initSecret)((acc, _) => nextSecret(acc))).sum

def part2(input: List[Long]): Int =
  val changesToPriceMaps =
    input.map { initSecret =>
      val prices = (1 to 2000)
        .foldLeft((initSecret, List(price(initSecret)))) { case ((secret, prices), _) =>
          val ns = nextSecret(secret)
          (ns, price(ns) :: prices)
        }
        ._2
        .reverse

      prices
        .sliding(5)
        .map { window =>
          val List(x0, x1, x2, x3, x4) = window
          (x1 - x0, x2 - x1, x3 - x2, x4 - x3) -> x4
        }
        .foldLeft(Map.empty[(Int, Int, Int, Int), Int]) { case (acc, (changes, price)) =>
          acc.get(changes) match
            case Some(existingPrice) => acc
            case None                => acc + (changes -> price)
        }
    }

  val changesToTotalPrice = mutable.HashMap.empty[(Int, Int, Int, Int), Int]
  changesToPriceMaps.foreach(_.foreach { case (k, v) =>
    changesToTotalPrice.updateWith(k) {
      case Some(existingValue) => Some(existingValue + v)
      case None                => Some(v)
    }
  })

  changesToTotalPrice.values.max
end part2

def price(secret: Long): Int =
  secret.toString.last.asDigit

def nextSecret(prev: Long): Long =
  val step1 = prune(mix(prev, prev * 64L))
  val step2 = prune(mix(step1, step1 / 32L))
  prune(mix(step2, step2 * 2048L))

def mix(a: Long, b: Long): Long = a ^ b

def prune(a: Long) = a % 16777216
