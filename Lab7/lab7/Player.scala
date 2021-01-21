package lab7

sealed trait Player {
  def unary_! : Player = this match {
    case Yellow => Blue
    case Blue => Yellow
  }
}
case object Yellow extends Player
case object Blue extends Player
