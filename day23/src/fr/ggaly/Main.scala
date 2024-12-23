package fr.ggaly

import scala.collection.mutable

@main def main(): Unit =
  val input = parseInput(Input.readLines())

  println(s"Part 1 result: ${part1(input)}")

  println(s"Part 2 result: ${part2(input)}")

def part1(input: List[Link]): Int =
  val directLinks = buildLinks(input)
  directLinks
    .flatMap { case (c, linked) =>
      linked
        .subsets(2)
        .flatMap { subset =>
          val i = subset.iterator
          val (c1, c2) = (i.next(), i.next())
          if (c.startsWith("t") || c1.startsWith("t") || c2.startsWith("t")) && directLinks(c1)
              .contains(c2)
          then Some(Set(c, c1, c2))
          else None
        }
    }
    .toSet
    .size
end part1

// Absolutely not optimised, but works (takes about 10 to 15 seconds)
def part2(input: List[Link]): String =
  val directLinks = buildLinks(input)

  var longestName = Vector.empty[String]
  directLinks
    .foreach { case (c, linked) =>
      linked
        .subsets()
        .foreach { subset =>
          if subset.size >= longestName.length then
            val isValid = subset.subsets(2).forall { pair =>
              val i = pair.iterator
              directLinks(i.next()).contains(i.next())
            }
            if isValid then longestName = (subset.toVector :+ c)
        }
    }

  longestName.sorted.mkString(",")
end part2

def buildLinks(input: List[Link]): Map[String, Set[String]] =
  val allComputers = input.flatMap(l => Iterator(l.c1, l.c2)).toSet
  val allLinks = input.toSet
  allComputers.iterator.map { c =>
    val linked = allLinks.collect {
      case link if link.contains(c) => if link.c1 == c then link.c2 else link.c1
    }
    c -> linked
  }.toMap
end buildLinks

def parseInput(input: List[String]): List[Link] =
  input.map { l =>
    val s = l.split('-')
    Link(s(0), s(1))
  }

object Link:
  // Order the two parameters, so that we don't have to worry about Link(aa,bb) being different from Link(bb,aa)
  def apply(c1: String, c2: String): Link =
    if c2 > c1 then new Link(c1, c2) else new Link(c2, c1)
final case class Link private (c1: String, c2: String):
  def contains(c: String): Boolean = c1 == c || c2 == c
