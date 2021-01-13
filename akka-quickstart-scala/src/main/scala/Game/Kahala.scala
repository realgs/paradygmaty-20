package Game

import Player.{HumanPlayer, AIPlayer, RandomPlayer}
import Server._
import akka.actor.{ActorSystem, Props}

object Kahala extends App {

  println("*************** Which type of game do you want to play ***************")
  println("First mentioned starts the game")
  println("1. Human vs. Human")
  println("2. Human vs RandomPlayer")
  println("3. RandomPlayer vs Human")
  println("4. RandomPlayer vs RandomPlayer")
  println("5. RandomPlayer vs AIPlayer")
  println("6. AIPlayer vs RandomPlayer")
  println("7. Human vs AIPlayer")
  println("8. AIPlayer vs Human")
  println("9. AIPlayer vs AIPlayer")


  def chooseGame: Int = {
    var gameNumber = 0

    while(gameNumber < 1 || gameNumber > 9) {
      println("Choose a number")
      try {
        gameNumber = scala.io.StdIn.readInt()
      } catch {
        case _ : NumberFormatException => gameNumber = 0
      }
    }
    gameNumber
  }

  val number = chooseGame

  number match {
    case 1 =>
      val system = ActorSystem("Kalaha")
      val gameBoard: Board = new Board
      val player1 = system.actorOf(Props(classOf[HumanPlayer], true), "player1")
      val player2 = system.actorOf(Props(classOf[HumanPlayer], false), "player2")
      val server = system.actorOf(Props(classOf[Server], player1, player2, gameBoard), "server")
      server ! Server.MakeMove

    case 2 =>
      val system = ActorSystem("Kalaha")
      val gameBoard: Board = new Board
      val player1 = system.actorOf(Props(classOf[HumanPlayer], true), "player1")
      val player2 = system.actorOf(Props(classOf[RandomPlayer], false), "player2")
      val server = system.actorOf(Props(classOf[Server], player1, player2, gameBoard), "server")
      server ! Server.MakeMove

    case 3 =>
      val system = ActorSystem("Kalaha")
      val gameBoard: Board = new Board
      val player1 = system.actorOf(Props(classOf[RandomPlayer], true), "player1")
      val player2 = system.actorOf(Props(classOf[HumanPlayer], false), "player2")
      val server = system.actorOf(Props(classOf[Server], player1, player2, gameBoard), "server")
      server ! Server.MakeMove

    case 4 =>
      val system = ActorSystem("Kalaha")
      val gameBoard: Board = new Board
      val player1 = system.actorOf(Props(classOf[RandomPlayer], true), "player1")
      val player2 = system.actorOf(Props(classOf[RandomPlayer], false), "player2")
      val server = system.actorOf(Props(classOf[Server], player1, player2, gameBoard), "server")
      server ! Server.MakeMove

    case 5 =>
      val system = ActorSystem("Kalaha")
      val gameBoard: Board = new Board
      val player1 = system.actorOf(Props(classOf[RandomPlayer], true), "player1")
      val player2 = system.actorOf(Props(classOf[AIPlayer], false), "player2")
      val server = system.actorOf(Props(classOf[Server], player1, player2, gameBoard), "server")
      server ! Server.MakeMove

    case 6 =>
      val system = ActorSystem("Kalaha")
      val gameBoard: Board = new Board
      val player1 = system.actorOf(Props(classOf[AIPlayer], true), "player1")
      val player2 = system.actorOf(Props(classOf[RandomPlayer], false), "player2")
      val server = system.actorOf(Props(classOf[Server], player1, player2, gameBoard), "server")
      server ! Server.MakeMove

    case 7 =>
      val system = ActorSystem("Kalaha")
      val gameBoard: Board = new Board
      val player1 = system.actorOf(Props(classOf[HumanPlayer], true), "player1")
      val player2 = system.actorOf(Props(classOf[AIPlayer], false), "player2")
      val server = system.actorOf(Props(classOf[Server], player1, player2, gameBoard), "server")
      server ! Server.MakeMove

    case 8 =>
      val system = ActorSystem("Kalaha")
      val gameBoard: Board = new Board
      val player1 = system.actorOf(Props(classOf[AIPlayer], true), "player1")
      val player2 = system.actorOf(Props(classOf[HumanPlayer], false), "player2")
      val server = system.actorOf(Props(classOf[Server], player1, player2, gameBoard), "server")
      server ! Server.MakeMove

    case 9 =>
      val system = ActorSystem("Kalaha")
      val gameBoard: Board = new Board
      val player1 = system.actorOf(Props(classOf[AIPlayer], true), "player1")
      val player2 = system.actorOf(Props(classOf[AIPlayer], false), "player2")
      val server = system.actorOf(Props(classOf[Server], player1, player2, gameBoard), "server")
      server ! Server.MakeMove

  }

}
