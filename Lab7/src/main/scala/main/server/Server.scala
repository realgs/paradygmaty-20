package main.server

import akka.actor.{Actor, ActorRef, PoisonPill, Terminated}
import main.Interface
import main.game.Game
import main.game.players.Player

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Random

class Server extends Actor {
  val moveTimeout: FiniteDuration = (30 seconds)

  var connected: Array[(ActorRef, Player)] = Array.empty;
  var currentGame: Game = null
  var currentPlayer: Player = null
  var currentClient: ActorRef = null
  var currentIndex: Int = -1

  var timerStart: Long = 0
  var timerEnd: Long = 0

  def connect(client: ActorRef, player: Player):Unit = {
    connected = connected :+ (client, player)
    context.watch(client)
    println(s"Connected $client")

    // If both players joined, start the game
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
  }

  def stop(): Unit = {
    currentGame.finish()
    for(client <- connected){
      client._1 ! PoisonPill
    }
    connected = Array.empty
    self ! PoisonPill
    context.system.terminate()
  }

  def startingPlayer: Int = {
    val rand = new Random()
    rand.nextInt(2)
  }

  def decideMove(): Unit = {
    broadcastMessage(Interface.turn(currentPlayer))
    currentClient ! PrintText(Interface.decideMove)
    currentClient ! DecideMove()
    timerStart = System.currentTimeMillis()
  }

  def makeMove(move: Int): Unit = {
    // Making move
    currentIndex = currentGame.move(currentIndex, move)

    // Sending updated board to clients
    sendUpdatedGame()

    // Switching player
    currentClient = connected(currentIndex)._1
    currentPlayer = currentGame.players(currentIndex)

    // Checking if game should be continued or finished
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
        timerEnd = System.currentTimeMillis()

        // Time to move exceeded, move will be chosen randomly
        if(timerEnd - timerStart > moveTimeout.toMillis){
          val availableMoves = currentPlayer.getAvailableMoves
          val rand = new Random()
          sender() ! MoveTimeout()
          makeMove(availableMoves(rand.nextInt(availableMoves.length)))
        }
        // Move made on time and correct
        else if(currentPlayer.getAvailableMoves.contains(move)){
          makeMove(move)
        }
        // Wrong move, asking for new one
        else {
          sender() ! WrongMove()
          sender() ! DecideMove()
        }
      }

    case msg => println("Unknown server message: " + msg)
  }
}
