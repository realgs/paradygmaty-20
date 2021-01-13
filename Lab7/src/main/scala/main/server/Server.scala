package main.server

import akka.actor.{Actor, ActorRef, PoisonPill, Terminated}
import akka.pattern.{AskTimeoutException, ask}
import main.Interface
import main.game.Game
import main.game.players.Player

import scala.concurrent.{Await, TimeoutException}
import scala.concurrent.duration.DurationInt
import scala.util.Random

class Server extends Actor {
  val moveTimeout = (30 seconds)

  var connected: Array[(ActorRef, Player)] = Array.empty;
  var currentGame: Game = null
  var currentPlayer: Player = null
  var currentClient: ActorRef = null
  var currentIndex: Int = -1

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

  def broadcastMessage(text: String): Unit = {
    for(client <- connected){
      client._1 ! PrintText(text)
    }
  }

  def start(): Unit = {
    // Creating new game
    currentGame = new Game(connected(0)._2, connected(1)._2)

    // Choosing player who move first
    currentIndex = startingPlayer
    currentClient = connected(currentIndex)._1
    currentPlayer = currentGame.players(currentIndex)

    // Starting the game
    currentGame.start()
    broadcastMessage(Interface.startInfo)
    sendUpdatedGame()
    decideMove()

//    broadcastMessage(Interface.board(currentGame.players))
//
//    while(currentGame.started) {
//      var chosenMove:Int = 0
//      try {
//        broadcastMessage(Interface.turn(currentPlayer))
//
//        currentClient ! PrintText(Interface.chooseMove)
//        val decidedMove = currentClient.ask(DecideMove)(moveTimeout).mapTo[Int]
//        chosenMove = Await.result(decidedMove, moveTimeout)
//
//        if(!currentPlayer.getAvailableMoves.contains(chosenMove)){
//          chosenMove = currentPlayer.getAvailableMoves(0)
//        }
//      }catch {
//        case _: AskTimeoutException => {
//          broadcastMessage(Interface.stop)
//          stop()
//          return
//        }
//        case _: TimeoutException => {
//          broadcastMessage(Interface.timeout)
//          broadcastMessage(Interface.finish)
//          broadcastMessage(Interface.winner(currentGame.players((currentPlayerIndex + 1) % currentGame.numberOfFields)))
//          stop()
//          return
//        }
//      }
//
//      currentPlayerIndex = currentGame.move(currentPlayerIndex, chosenMove)
//      currentClient = connected(currentPlayerIndex)._1
//      currentPlayer = connected(currentPlayerIndex)._2
//
//      broadcastMessage(Interface.board(currentGame.players))
//    }
//
//    broadcastMessage(Interface.finish)
//    broadcastMessage(Interface.points(currentGame.players))
//    broadcastMessage(Interface.winner(currentGame.winner))

  }

  def stop(): Unit = {
    currentGame.finish()
    for(client <- connected){
      client._1 ! PoisonPill
    }
    self ! PoisonPill
  }

  def startingPlayer: Int = {
    val rand = new Random()
    rand.nextInt(2)
  }

  def decideMove(): Unit = {
    broadcastMessage(Interface.turn(currentPlayer))
    currentClient ! PrintText(Interface.decideMove)
    currentClient ! DecideMove()
  }

  def makeMove(move: Int): Unit = {
    currentIndex = currentGame.move(currentIndex, move)

    sendUpdatedGame()

    currentClient = connected(currentIndex)._1
    currentPlayer = currentGame.players(currentIndex)

    if(currentGame.started){
      decideMove()
    }else {
      broadcastMessage(Interface.finish)
      broadcastMessage(Interface.points(currentGame.players))
      broadcastMessage(Interface.winner(currentGame.winner))
      stop()
    }

  }

  def sendUpdatedGame(): Unit = {
    for(client <- connected){
      val (actor, player) = client
      actor ! UpdateData(player.fields, player.base)
      actor ! PrintText(Interface.board(if(player == currentGame.players(0)) currentGame.players else currentGame.players.reverse))
    }
  }

  override def receive: Receive = {
    case Connected(player: Player) => connect(sender(), player)
    case Terminated(client: ActorRef) => disconnect(client)

    case StartGame => start()
    case MoveDecided(move: Int) =>
      if(sender() == currentClient) {
        if(currentPlayer.getAvailableMoves.contains(move)){
          makeMove(move)
        }else {
          sender() ! WrongMove()
          sender() ! DecideMove()
        }
      }

    case msg => println("Unknown server message: " + msg)
  }
}
