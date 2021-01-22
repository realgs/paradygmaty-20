package Game

import akka.actor._
import Board.Board
import actors._
import engine.{BasicEngine, KalahaEngine}

class Game
{
  val system = ActorSystem()

  def run(): Unit =
  {
    val mode: Int = askForMode()
    mode match
      {
      case 1 => play_PvP()
      case 2 => play_PvB()
      case 3 => play_BvB()
      case _ => system.terminate()
    }
  }

  private def askForMode(): Int =
  {
    println("Hello! Choose a game mode:")
    println("1. Player vs Player")
    println("2. Player vs Bot")
    println("3. Bot vs Bot")
    scala.io.StdIn.readInt()
  }

  private def play_PvP(): Unit =
  {
    println("First Player Username: ")
    val username0 = scala.io.StdIn.readLine()
    println("Second Player Username: ")
    val username1 = scala.io.StdIn.readLine()

    val player0: ActorRef = system.actorOf(Human.props(username0, 0))
    val player1: ActorRef = system.actorOf(Human.props(username1, 1))

    startGame(player0, player1)
  }

  private def play_PvB(): Unit =
  {
    println("Choose difficulty!")
    println("1. Noob")
    println("2. Intermediate")
    println("3. Advanced")
    val difficulty = scala.io.StdIn.readInt()

    var depth = 0
    difficulty match
      {
      case 1 => depth = 4
      case 2 => depth = 6
      case 3 => depth = 8
      case _ =>
        system.terminate()
        throw new Exception(s"Wrong difficulty: $difficulty")
    }

    println("\nUsername: ")
    val username = scala.io.StdIn.readLine()

    val player: ActorRef = system.actorOf(Human.props(username, 0))

    val engine: KalahaEngine = new BasicEngine(1, depth)
    val bot: ActorRef = system.actorOf(Bot.props(1, engine))

    startGame(player, bot)
  }

  private def play_BvB(): Unit =
  {
    println("Choose depth of first bot: ")
    val difficulty0 = scala.io.StdIn.readInt()
    println("Choose depth of second bot: ")
    val difficulty1 = scala.io.StdIn.readInt()

    if (difficulty0 <= 0 || difficulty1 <= 0)
      {
        system.terminate()
        throw new Exception(s"Wrong difficulty: dif1: $difficulty0 dif2: $difficulty1")
      }


    val engine0: KalahaEngine = new BasicEngine(0, difficulty0)
    val bot0: ActorRef = system.actorOf(Bot.props(0, engine0))
    val engine1: KalahaEngine = new BasicEngine(1, difficulty1)
    val bot1: ActorRef = system.actorOf(Bot.props(1, engine1))

    startGame(bot0, bot1)
  }

  private def startGame(player0: ActorRef, player1: ActorRef): Unit =
  {
    val board: Board = new Board
    val server: ActorRef = system.actorOf(Server.props(board, Array(player0, player1)))
    server ! Server.PlayGame(0)
  }
}
