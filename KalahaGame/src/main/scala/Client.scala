import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

import scala.io.StdIn.readInt

object Client {
  var gameState: GameState = GameState()

  private def refreshDisplay(name: String): Unit = {
    println(s"[$name] Refreshing state...")
    println(gameState)
  }

  def apply(name: String = "unnamedClient"): Behavior[ServerResponse] = {
    Behaviors.receive { (ctx, msg) =>
      msg match {
        case ExpectMove(server) =>
          printf("[%s] Move expected: ", name)
          val moveIndex = readInt()

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
