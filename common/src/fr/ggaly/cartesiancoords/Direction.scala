package fr.ggaly.cartesiancoords

enum Direction(val x: Int, val y: Int) extends CoordsLike:
  case Up extends Direction(0, -1)
  case Right extends Direction(1, 0)
  case Down extends Direction(0, 1)
  case Left extends Direction(-1, 0)

  def turnRight: Direction = this match
    case Up    => Right
    case Right => Down
    case Down  => Left
    case Left  => Up

  def turnLeft: Direction = this match
    case Up    => Left
    case Right => Up
    case Down  => Right
    case Left  => Down
end Direction
