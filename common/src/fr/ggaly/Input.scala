package fr.ggaly

import scala.io.Source

object Input:
  def read(name: String = "input.txt"): String =
    val is = ClassLoader.getSystemClassLoader.getResourceAsStream(name)
    new String(is.readAllBytes(), "UTF-8")

  def readLines(name: String = "input.txt"): List[String] =
    val is = ClassLoader.getSystemClassLoader.getResourceAsStream(name)
    val src = Source.fromInputStream(is, "UTF-8")
    src.getLines().toList
end Input
