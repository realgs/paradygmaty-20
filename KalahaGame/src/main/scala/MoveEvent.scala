import akka.actor.typed.ActorRef

sealed trait Command
final case class InitiateServer(gameVariant: Int, engineClientCount: Int) extends Command
final case class Move(position: Int, sender: ActorRef[ServerResponse]) extends Command


sealed trait ServerResponse
final case class RefreshState(gs: GameState) extends ServerResponse

final case class ExpectMove(sender: ActorRef[Command]) extends ServerResponse
final case class ExpectEngineMove(sender: ActorRef[Command], yourTurn: Int) extends ServerResponse

final case class TimeExpired(sender: ActorRef[Command]) extends ServerResponse

final case class InvalidMove(relativeIndex: Int, sender: ActorRef[Command]) extends ServerResponse
final case class PromptMessage(msg: String) extends ServerResponse
final case class GameFinished(msg: String) extends ServerResponse
