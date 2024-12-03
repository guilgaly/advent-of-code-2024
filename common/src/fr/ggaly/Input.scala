package fr.ggaly

import scala.io.Source

object Input:
  def read: String =
    val is = ClassLoader.getSystemClassLoader.getResourceAsStream("input.txt")
    new String(is.readAllBytes(), "UTF-8")

  def readLines: List[String] =
    val is = ClassLoader.getSystemClassLoader.getResourceAsStream("input.txt")
    val src = Source.fromInputStream(is, "UTF-8")
    src.getLines().toList
end Input
