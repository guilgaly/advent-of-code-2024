package fr.ggaly

import scala.io.Source

object Input:
  def readLines: List[String] =
    val is = ClassLoader.getSystemClassLoader.getResourceAsStream("input.txt")
    val src = Source.fromInputStream(is, "UTF-8")
    src.getLines().toList
