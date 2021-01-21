package lab7

import scala.util.Random

class Position private(var turn: Player, _holes: Array[Int]) {
  val holes: Array[Int] = _holes.clone()

  def firstHole: Int = turn match {
    case Yellow => 0
    case Blue => 7
  }

  def this(position: Position, _move: Int) {
    this(position.turn , position.holes)
    val tmp = this.holes(_move)
    this.holes(_move) = 0
    var i = _move
    for (_ <- 0 until tmp) {
      i = (i+1) % 14
      this.holes(i) += 1
    }

    if (holes(i) == 1 && (i < 6 && turn == Yellow || i >= 7 && i < 13 && turn == Blue)) {
      holes(6 + firstHole) += holes(12 - i)
      holes(12 - i) = 0
    }

    if ((i != 6 || turn == Blue) && (i != 13 || turn == Yellow)) turn = !turn

    if ((0 to 5).forall(i => holes(i) == 0)) {
      holes(13) += (7 to 12).map(i => holes(i)).sum
      (7 to 12).foreach(i => holes(i) = 0)
    }
    if ((7 to 12).forall(i => holes(i) == 0)) {
      holes(6) += (0 to 5).map(i => holes(i)).sum
      (0 to 5).foreach(i => holes(i) = 0)
    }
  }

  class EmptyHoleException extends IllegalArgumentException("this hole is empty")

  def next(_move: Int): Position = {
    if (_move <= 0 || _move > 6) throw new IllegalArgumentException("move must be between 1 and 6")
    val __move = _move + firstHole - 1
    if (holes(__move) == 0) throw new EmptyHoleException()

    new Position(this, __move)
  }

  override def toString: String = {
    def prettyInt(i: Int): String = (if (i < 10) " " else "") + i
    var s = Console.BLUE
    s += (if (turn == Blue) "^ " else "  ")
    for(i <- (7 to 12).reverse) s += prettyInt(holes(i)) + " "
    s += "\r\n" + prettyInt(holes(13)) + "                  " + Console.YELLOW + holes(6) + "\r\n"
    s += (if (turn == Yellow) "^ " else "  ")
    for(i <- 0 to 5) s += prettyInt(holes(i)) + " "
    s + Console.RESET
  }

  def winner: Option[Player] =
    if (holes(6) == 36 && holes(13) == 36) Some(!turn)
    else if (holes(6) > 36) Some(Yellow)
    else if (holes(13) > 36) Some(Blue)
    else None

  def finished: Boolean = holes(6) + holes(13) == 72

  def highground: Int = highground(turn)
  def highground(_turn: Player): Int = if (_turn == Yellow) holes(6) - holes(13)
  else holes(13) - holes(6)

  def modifiedHighground(color: Player): Int = {
    if (winner.contains(color)) highground(color) + 72
    else if (winner.contains(!color)) highground(color) - 72
    else highground(color)
  }

  def randomMove: Int = {
    var move: Int = -1
    do {
      move = Random.nextInt(6)
    } while(holes( move + firstHole) == 0)
    move + 1
  }
}

object Position {
  def apply(): Position = new Position(Yellow, Array(6,6,6,6,6,6,0,6,6,6,6,6,6,0))
}
