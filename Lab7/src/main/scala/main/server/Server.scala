package main.server

import akka.actor.{Actor, ActorRef, PoisonPill, Terminated}
import akka.pattern.{AskTimeoutException, ask}
import main.Interface
import main.game.Game
import main.game.players.Player

import scala.concurrent.{Await, TimeoutException}
import scala.concurrent.duration.DurationInt

class Server extends Actor {
  val moveTimeout = (30 seconds)

  var connected: Array[(ActorRef, Player)] = Array.empty;
  var currentGame: Game = null

  def connect(client: ActorRef, player: Player):Unit = {
    connected = connected :+ (client,player)
    context.watch(client)
    println(s"Connected $client:$player")

    if(connected.length == 2) {
      self ! StartGame
    }
  }

  def disconnect(client: ActorRef): Unit = {
    println(s"Disconnected client: $client")

    stop();
  }

  def broadcast(text: String): Unit = {
    for(client <- connected){
      client._1 ! PrintText(text)
    }
  }

  def start(): Unit = {
    currentGame = new Game(connected(0)._2, connected(1)._2)

    currentGame.start()

    broadcast(Interface.startInfo)

    var currentClient = connected(0)._1
    var currentPlayer = connected(0)._2
    var currentPlayerIndex = 0

    broadcast(Interface.board(currentGame.players))

    while(currentGame.started) {
      var chosenMove:Int = 0
      try {
        broadcast(Interface.turn(currentPlayer))

        currentClient ! PrintText(Interface.chooseMove)
        val decidedMove = currentClient.ask(DecideMove)(moveTimeout).mapTo[Int]
        chosenMove = Await.result(decidedMove, moveTimeout)

        if(!currentPlayer.getAvailableMoves.contains(chosenMove)){
          chosenMove = currentPlayer.getAvailableMoves(0)
        }
      }catch {
        case _: AskTimeoutException => {
          broadcast(Interface.stop)
          stop()
          return
        }
        case _: TimeoutException => {
          broadcast(Interface.timeout)
          broadcast(Interface.finish)
          broadcast(Interface.winner(currentGame.players((currentPlayerIndex + 1) % currentGame.numberOfFields)))
          stop()
          return
        }
      }

      currentPlayerIndex = currentGame.move(currentPlayerIndex, chosenMove)
      currentClient = connected(currentPlayerIndex)._1
      currentPlayer = connected(currentPlayerIndex)._2

      broadcast(Interface.board(currentGame.players))
    }

    broadcast(Interface.finish)
    broadcast(Interface.points(currentGame.players))
    broadcast(Interface.winner(currentGame.winner))

  }

  def stop(): Unit = {

    currentGame.finish()

    self ! PoisonPill
  }

  override def receive: Receive = {
    case Connected(player: Player) => connect(sender(), player)
    case Terminated(client: ActorRef) => disconnect(client)

    case StartGame => start()

    case _ => println("Unknown server message")
  }
}
