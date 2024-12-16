package fr.ggaly

import scala.annotation.tailrec

object Part2:
  def apply(warehouseInput: List[String], movesInput: String): Long =
    val initialState = parseWarehouse(warehouseInput)
    val moves = parseMoves(movesInput)
    val endState = moves.foldLeft(initialState)((state, dir) => state.next(dir))
    endState.crates.map(_.gps).sum

  private def parseWarehouse(input: List[String]): State =
    input.zipWithIndex
      .flatMap((line, y) => line.zipWithIndex.map((c, x) => (c, Coords(2 * x, y))))
      .foldLeft(State(Set.empty, Set.empty, Coords(0, 0))) {
        case (state, ('@', coords)) => state.copy(robot = coords)
        case (state, ('#', coords)) => state.copy(walls = state.walls + coords + coords.move(Right))
        case (state, ('O', coords)) =>
          state.copy(crates = state.crates + Crate(coords, coords.move(Right)))
        case (state, _) => state
      }

  final case class Crate(a: Coords, b: Coords):
    def gps: Long = a.gps
    def move(dir: Direction): Crate = Crate(a.move(dir), b.move(dir))

  final case class State(walls: Set[Coords], crates: Set[Crate], robot: Coords):
    private def findCrate(target: Coords): Option[Crate] =
      crates.find(c => c.a == target || c.b == target)

    private def findCrates(targets: Set[Crate]): Set[Crate] =
      val allCoords = targets.flatMap(t => Iterator(t.a, t.b))
      crates.filter(c => allCoords.contains(c.a) || allCoords.contains(c.b))

    @tailrec
    private def moveCratesTo(targets: Set[Crate], dir: Direction): Option[State] =
      if targets.exists(t => walls.contains(t.a) || walls.contains(t.b)) then None
      else
        val collisions = findCrates(targets)
        if collisions.isEmpty then Some(copy(crates = crates ++ targets))
        else
          copy(crates = crates -- collisions ++ targets)
            .moveCratesTo(collisions.map(_.move(dir)), dir)

    def next(dir: Direction): State =
      val rm = robot.move(dir)
      if walls.contains(rm) then this
      else
        findCrate(rm) match
          case Some(crate) =>
            copy(crates = crates - crate, robot = rm)
              .moveCratesTo(Set(crate.move(dir)), dir)
              .getOrElse(this)
          case None => copy(robot = rm)
      end if
    end next
  end State
end Part2
