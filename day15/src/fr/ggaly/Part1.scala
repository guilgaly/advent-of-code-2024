package fr.ggaly

import scala.annotation.tailrec

object Part1:
  def apply(warehouseInput: List[String], movesInput: String): Long =
    val initialState = parseWarehouse(warehouseInput)
    val moves = parseMoves(movesInput)
    val endState = moves.foldLeft(initialState)((state, dir) => state.next(dir))
    endState.crates.map(_.gps).sum

  private def parseWarehouse(input: List[String]): State =
    input.zipWithIndex
      .flatMap((line, y) => line.zipWithIndex.map((c, x) => (c, Coords(x, y))))
      .foldLeft(State(Set.empty, Set.empty, Coords(0, 0))) {
        case (state, ('@', coords)) => state.copy(robot = coords)
        case (state, ('#', coords)) => state.copy(walls = state.walls + coords)
        case (state, ('O', coords)) => state.copy(crates = state.crates + coords)
        case (state, _)             => state
      }

  final case class State(walls: Set[Coords], crates: Set[Coords], robot: Coords):
    @tailrec
    private def moveCrateTo(target: Coords, dir: Direction): Option[State] =
      if walls.contains(target) then None
      else if crates.contains(target) then moveCrateTo(target.move(dir), dir)
      else Some(copy(crates = crates + target))

    def next(dir: Direction): State =
      val rm = robot.move(dir)
      if walls.contains(rm) then this
      else if crates.contains(rm) then
        copy(crates = crates - rm, robot = rm).moveCrateTo(rm.move(dir), dir).getOrElse(this)
      else copy(robot = rm)
  end State
end Part1
