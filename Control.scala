package Kalaha

import akka.actor.ActorRef

abstract class Control

//Case class for Server to communicate with players
case class Start(player: ActorRef) extends Control
case class Connect() extends Control
case class MakeMove(holeNumber: Int, playerNumber: Int) extends Control
case class EndGame() extends Control
case class EndTime() extends Control
case class TimesUp(playerNumber: Int) extends Control

//Case class for Humans and ComputerPlayers to communicate with server
case class TurnAgainPlayer(board: Board) extends Control
case class InformInvalidMove(board: Board, playerNumber: Int) extends Control
case class ReturnPlayerNumber(playerNumber: Int) extends Control
case class RequireMove(playerNumber: Int, board: Board) extends Control
case object Disconnect extends Control
