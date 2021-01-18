package L7

abstract class Messages

//server
case class Connect(playerToken: Int) extends Messages
case class MakeMove(houseNumber :Int, playerToken :Int) extends Messages
case class PrintServer(a :  Int) extends Messages
case class timeIsOver() extends Messages

//player
case class MoveNeeded(board: Board) extends Messages
case class Disconnect() extends Messages
case class PrintPlayer(a :  Int) extends Messages



