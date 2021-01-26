import GameServer.PlayerData
import akka.actor.{ActorRef, ActorSystem}

import scala.io.StdIn

object KalahGame extends App {

  private val actor_system = ActorSystem()

  print(s"Choose mode for player 1 (0 - computer, 1 - real player) = ")
  val first_player = PlayerData(7, List.range(1, 7), actor_system.actorOf(Player.props(1, 7, List.range(1, 7), StdIn.readInt())))

  print(s"Choose mode for player 2 (0 - computer, 1 - real player) = ")
  val second_player = PlayerData(14, List.range(8, 14), actor_system.actorOf(Player.props(2, 14, List.range(8, 14), StdIn.readInt())))

  val server = actor_system.actorOf(GameServer.props(6, first_player, second_player))
  server ! GameServer.Start

}
