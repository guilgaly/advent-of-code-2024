package fr.ggaly

import scala.collection.mutable

@main def main(): Unit =
  val input = Input.read()

  println(s"Part 1 result: ${part1(input)}")

  println(s"Part 2 result: ${part2(input)}")

def part1(input: String): Long =
  val diskMap = input.map(_.asDigit).toList

  val disk = mutable.Buffer.empty[Int]
  var isFreeSpace = false
  var fileId = 0
  for entry <- diskMap do
    if isFreeSpace then for _ <- 0 until entry do disk.append(-1)
    else
      for _ <- 0 until entry do disk.append(fileId)
      fileId += 1
    isFreeSpace = !isFreeSpace

  var i = 0
  var j = disk.length - 1
  while i < j do
    if disk(i) >= 0 then i += 1
    else if disk(j) == -1 then j -= 1
    else
      disk(i) = disk(j)
      disk(j) = -1
    end if

  disk.zipWithIndex
    .takeWhile { case (b, _) => b >= 0 }
    .map { case (b, idx) => (b * idx).toLong }
    .sum
end part1

def part2(input: String): Long =
  val diskMap = input.map(_.asDigit).toList

  val files = mutable.Buffer.empty[(Int, Int)] // fileId -> (startIdx, endIdx)
  var isFile = true
  var currIdx = 0
  for entry <- diskMap do
    if isFile then
      files.append((currIdx, currIdx + entry - 1))
    currIdx += entry
    isFile = !isFile
  val isFree = mutable.Buffer.fill(currIdx)(true)
  for case (start, end) <- files do
    for i <- start to end do isFree(i) = false

  val maxFileId = files.size - 1
  for fileId <- maxFileId.to(0, -1) do
    val (start, end) = files(fileId)
    val fileLength = end - start + 1
    val newStart = isFree.iterator
      .take(start)
      .sliding(fileLength)
      .indexWhere(_.forall(identity))
    if newStart >= 0 then
      files(fileId) = (newStart, newStart + fileLength - 1)
      for i <- start to end do isFree(i) = true
      for i <- newStart until newStart + fileLength do isFree(i) = false
  end for

  files.zipWithIndex.map { case ((start, end), fileId) =>
    (start to end).map(idx => (fileId * idx).toLong).sum
  }.sum

end part2

private def printDisk(disk: mutable.Buffer[Int]): Unit =
  println(disk.map(b => if b == -1 then "." else b.toString).mkString)
