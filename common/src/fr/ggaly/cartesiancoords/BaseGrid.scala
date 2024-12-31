package fr.ggaly.cartesiancoords

trait BaseGrid:
  def width: Int
  def height: Int
  def blocked: Set[Coords] = Set.empty

  def contains(point: Coords): Boolean =
    point.x >= 0 && point.x < width && point.y >= 0 && point.y < height

  def isAccessible(point: Coords): Boolean =
    contains(point) && !blocked.contains(point)

  def accessibleNeighbours(point: Coords): Iterator[Coords] =
    point.neighbours.filter(isAccessible)

  def blockedNeighbours(point: Coords): Iterator[Coords] =
    point.neighbours.filterNot(isAccessible)

end BaseGrid
