import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorSystem, Behavior}

object Main extends App {
  val gameVariant = 4
  val timeToMoveSeconds = 10
  val numEnginePlayers = 2

  printf("Starting game... Seeds per house = %s\n", gameVariant)
  printf("Time to move: %ss\n", timeToMoveSeconds)
  printf("Number of engine players: %s\n", numEnginePlayers)

  val server: Behavior[Command] = Server(timeToMoveSeconds)

  ActorSystem[Nothing](Behaviors.setup[Nothing] { ctx =>
    val startRef = ctx.spawn(server, "serverStart")
    ctx.watch(startRef)

    startRef ! InitiateServer(gameVariant, numEnginePlayers)

    Behaviors.empty
  }, "server")
}
