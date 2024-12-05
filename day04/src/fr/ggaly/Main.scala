package fr.ggaly

@main def main(): Unit =
  val input = Input.readLines().map(line => line.toCharArray).toArray

  println(s"Part 1 result: ${part1(input)}")

  println(s"Part 2 result: ${part2(input)}")

def part1(input: Array[Array[Char]]): Int =
  val maxY = input.length - 1
  val maxX = input(0).length - 1

  var count = 0
  for
    y <- 0 to maxY
    x <- 0 to maxX
  do
    if input(y)(x) == 'X' then
      // Horizontal
      if x <= maxX - 3 && input(y)(x + 1) == 'M' && input(y)(
          x + 2,
        ) == 'A' && input(y)(x + 3) == 'S'
      then count += 1
      if x >= 3 && input(y)(x - 1) == 'M' && input(y)(x - 2) == 'A' && input(y)(
          x - 3,
        ) == 'S'
      then count += 1
      // Vertical
      if y <= maxY - 3 && input(y + 1)(x) == 'M' && input(y + 2)(
          x,
        ) == 'A' && input(y + 3)(x) == 'S'
      then count += 1
      if y >= 3 && input(y - 1)(x) == 'M' && input(y - 2)(x) == 'A' && input(
          y - 3,
        )(x) == 'S'
      then count += 1
      // Diagonal
      if x <= maxX - 3 && y <= maxY - 3 && input(y + 1)(x + 1) == 'M' && input(
          y + 2,
        )(x + 2) == 'A' && input(y + 3)(x + 3) == 'S'
      then count += 1
      if x <= maxX - 3 && y >= 3 && input(y - 1)(x + 1) == 'M' && input(y - 2)(
          x + 2,
        ) == 'A' && input(y - 3)(x + 3) == 'S'
      then count += 1
      if x >= 3 && y <= maxY - 3 && input(y + 1)(x - 1) == 'M' && input(y + 2)(
          x - 2,
        ) == 'A' && input(y + 3)(x - 3) == 'S'
      then count += 1
      if x >= 3 && y >= 3 && input(y - 1)(x - 1) == 'M' && input(y - 2)(
          x - 2,
        ) == 'A' && input(y - 3)(x - 3) == 'S'
      then count += 1
    end if
  end for

  count
end part1

def part2(input: Array[Array[Char]]): Int =
  val maxY = input.length - 1
  val maxX = input(0).length - 1

  var count = 0
  for
    y <- 1 until maxY
    x <- 1 until maxX
  do
    if input(y)(x) == 'A' then
      val topLeft = input(y - 1)(x - 1)
      val downLeft = input(y + 1)(x - 1)
      val topRight = input(y - 1)(x + 1)
      val downRight = input(y + 1)(x + 1)

      if ((topLeft == 'M' && downRight == 'S') || (topLeft == 'S' && downRight == 'M')) && ((downLeft == 'M' && topRight == 'S') || (downLeft == 'S' && topRight == 'M'))
      then count += 1
    end if
  end for

  count
end part2
