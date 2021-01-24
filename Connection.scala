abstract class Connection

final case class Connect(playerID: Int) extends Connection
final case class Disconnect() extends Connection
final case class MakeMove() extends Connection

final case class Move(gameBoard: GameBoard) extends Connection
