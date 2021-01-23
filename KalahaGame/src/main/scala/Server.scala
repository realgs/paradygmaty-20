import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors

object Server {
  private var gameState: GameState = GameState()
  private var currentTurn: ActorRef[ServerResponse] = _

  private var south: ActorRef[ServerResponse] = _
  private var north: ActorRef[ServerResponse] = _

  private var timeToMoveMillis: Long = 10 * 1000
  private var timeStart: Long = 0

  implicit private def bool2int(b: Boolean): Int = if (b) 1 else 0

  private def changeTurn(): Unit = {
    if (currentTurn == south) currentTurn = north else currentTurn = south
  }

  def apply(timeToMoveSeconds: Int): Behavior[Command] = {
    timeToMoveMillis = timeToMoveSeconds * 1000
    Behaviors.receive { (ctx, msg) =>
      msg match {
        case InitiateServer(gameVariant, engineClientCount) =>
          gameState = GameState(gameVariant)
          println("[Server] Initiating...")

          if (engineClientCount == 0) {
            south = ctx.spawn(Client("South"), name = "SouthClient")
            north = ctx.spawn(Client("North"), name = "NorthClient")
          } else if (engineClientCount == 1) {
            south = ctx.spawn(Client("South"), name = "SouthClient")
            north = ctx.spawn(EngineClient("North_engine", 1), name = "NorthClient")
          } else {
            south = ctx.spawn(EngineClient("South_engine", 0), name = "SouthClient")
            north = ctx.spawn(EngineClient("North_engine", 1), name = "NorthClient")
          }

          south ! RefreshState(gameState)
          north ! RefreshState(gameState)

          currentTurn = south
          south ! ExpectMove(ctx.self)

          timeStart = System.currentTimeMillis()

          Behaviors.same
        case Move(relativeIndex, sender) =>
          if (sender == currentTurn) {
            val turnIdent = bool2int(sender == north)

            if (System.currentTimeMillis() - timeStart > timeToMoveMillis) {
              currentTurn ! PromptMessage("\tYou missed the move! Game ended!")
              currentTurn ! GameFinished("\tGame lost by time!")
              changeTurn()
              currentTurn ! GameFinished("\tGame won by time!")

              Behaviors.stopped
            }

            if (gameState.isMoveCorrect(relativeIndex)(turnIdent)) {
              var moveGranted = gameState.move(relativeIndex)(turnIdent)
              val isGameEnded = gameState.checkGameEnded
              printf("[Server] %s has made a move\n", sender)

              if (isGameEnded) {
                println("[Server] Game ended")
                moveGranted = false
              }

              if (!moveGranted) {
                changeTurn()
              } else {
                printf("[Server] Player %s fulfilled special condition and is given another move\n", sender)
              }

              south ! RefreshState(gameState)
              north ! RefreshState(gameState)

              if (!isGameEnded) {
                currentTurn ! ExpectMove(ctx.self)
                timeStart = System.currentTimeMillis()
              } else {
                val result = gameState.getWinner

                if (result == -1) {
                  south ! GameFinished("\tGame drawn!")
                  north ! GameFinished("\tGame drawn!")
                  Behaviors.stopped
                }

                val (winner, loser) = if (result == 0) (south, north) else (north, south)

                winner ! GameFinished("\tGame won!")
                loser ! GameFinished("\tGame lost!")

                Behaviors.stopped
              }
            } else {
              sender ! PromptMessage(s"\tMove from index=$relativeIndex is invalid")
              sender ! ExpectMove(ctx.self)
            }
          } else {
            sender ! PromptMessage("Not your turn!")
          }
          Behaviors.same
      }
    }
  }
}
