package fr.ggaly

import scala.collection.mutable

@main def main(): Unit =
  val input = Input.read()

  println(s"Part 1 result: ${part1(input)}")

  println(s"Part 2 result: ${part2Naive(input)} (naive)")

  println(s"Part 2 result: ${part2Optimized(input)} (optimized)")
end main

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

def part2Naive(input: String): Long =
  val diskMap = input.map(_.asDigit).toList
  val totalLength = diskMap.sum

  val files = mutable.Buffer.empty[(Int, Int)] // fileId -> (startIdx, endIdx)
  var isFile = true
  var currIdx = 0
  for entry <- diskMap do
    if isFile then files.append((currIdx, currIdx + entry - 1))
    currIdx += entry
    isFile = !isFile
  val isFree = mutable.Buffer.fill(currIdx)(true)
  for case (start, end) <- files do for i <- start to end do isFree(i) = false

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
end part2Naive

def part2Optimized(input: String): Long =
  val diskMap = input.map(_.asDigit).toList

  // index = fileId, value = (startIdx, length)
  val files = mutable.Buffer.empty[(Int, Int)]
  // value = (startIdx, length)
  val freeSpaces = mutable.ArrayDeque.empty[(Int, Int)]

  var isFile = true
  var currIdx = 0
  for entryLength <- diskMap do
    if isFile then files.append((currIdx, entryLength))
    else if entryLength > 0 then
      val _ = freeSpaces.append((currIdx, entryLength))
    currIdx += entryLength
    isFile = !isFile

  for ((fileStart, fileLength), fileId) <- files.zipWithIndex.reverseIterator do
    freeSpaces.iterator
      .takeWhile { case (freeSpaceStart, _) => freeSpaceStart < fileStart }
      .zipWithIndex
      .find { case ((_, freeSpaceLength), _) =>
        freeSpaceLength >= fileLength
      } match
      case Some(((freeSpaceStart, freeSpaceLength), freeSpaceIdx)) =>
        files(fileId) = (freeSpaceStart, fileLength)
        if freeSpaceLength > fileLength then
          freeSpaces.update(
            freeSpaceIdx,
            (freeSpaceStart + fileLength, freeSpaceLength - fileLength),
          )
        else freeSpaces.remove(freeSpaceIdx)
      case None =>
  end for

  files.zipWithIndex.map { case ((start, length), fileId) =>
    (start until start + length).map(idx => (fileId * idx).toLong).sum
  }.sum
end part2Optimized

private def printDisk(disk: mutable.Buffer[Int]): Unit =
  println(disk.map(b => if b == -1 then "." else b.toString).mkString)

private def printFiles(
    files: mutable.Buffer[(Int, Int)],
    totalLength: Int,
): Unit =
  val arr = Array.fill(totalLength)('.')
  for ((start, length), fileId) <- files.zipWithIndex do
    for i <- start until start + length do
      arr(i) = fileId.toString.toCharArray.head
  println(arr.mkString)
end printFiles
