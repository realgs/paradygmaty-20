import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

object EngineClient {
  var gameState: GameState = GameState()

  private def refreshDisplay(name: String): Unit = {
    println(s"[$name] Refreshing state...")
    println(gameState)
  }

  def apply(name: String = "aiClient", sideIdent: Int): Behavior[ServerResponse] = {
    Behaviors.receive { (ctx, msg) =>
      msg match {
        case ExpectMove(server) =>
          println(s"[$name]: Making move...")
          val moveIndex = Logic.getMove(gameState)(sideIdent)
          println(s"[$name]: Moved from index=$moveIndex")

          server ! Move(moveIndex, ctx.self)

          Behaviors.same
        case RefreshState(gs) =>
          gameState = gs
          refreshDisplay(name)
          Behaviors.same
        case PromptMessage(mgs) =>
          println(mgs)
          Behaviors.same
        case GameFinished(msg) =>
          printf("[%s] %s\n", name, msg)
          Behaviors.stopped
      }
    }
  }
}
