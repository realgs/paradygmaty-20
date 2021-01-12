package Kalaha

import akka.actor.{Actor, ActorRef, PoisonPill}

class ComputerPlayer(server: ActorRef) extends Actor {

  var playerNumber: Int = _
  var board: Board = _

  server ! Connect()

  def receive: Receive = {
    case ReturnPlayerNumber(number) =>
      playerNumber = number
      println("Player: " + playerNumber + " get the number")
      server ! Start(self)

    case RequireMove(number, newBoard) =>
      if(playerNumber == number) {
        var chosenHole = selectHole(newBoard, playerNumber)
        if(newBoard.getStonesNumber(chosenHole) <= 0) chosenHole = ifInvalidMove(newBoard, playerNumber)
        println("Player: " + playerNumber + " chose hole number: " + chosenHole)
        server ! MakeMove(chosenHole, playerNumber)
      }

    case InformInvalidMove(newBoard, number) =>
      if(playerNumber == number) {
        val chosenHole = ifInvalidMove(newBoard, playerNumber)
        if(chosenHole == -1) server ! TimesUp(playerNumber)
        else server ! MakeMove(chosenHole, playerNumber)
      }

    case TurnAgainPlayer(newBoard) =>
      println("Player: " + playerNumber + " moves again.")
      var chosenHole = selectHole(newBoard, playerNumber)
      if(newBoard.getStonesNumber(chosenHole) <= 0) chosenHole = ifInvalidMove(newBoard, playerNumber)
      println("Player: " + playerNumber + " chose hole number: " + chosenHole)
      server ! MakeMove(chosenHole, playerNumber)

    case Disconnect =>
      println("Disconnect - computer player number: " + playerNumber)
      self ! PoisonPill

  }

  def selectHole(newBoard: Board, playerNumber: Int): Int = {
    board = newBoard
    val tree = new DecisionTree(board, playerNumber)
    tree.createFirstLevel()
    tree.createTree()
    val chosenHole = tree.findBestDecision(tree.createChoiceQueue(tree.getRoot()))
    chosenHole
  }

  def ifInvalidMove(newBoard: Board, playerNumber: Int): Int = {
    board = newBoard
    for(x <- 0 to 5) {
      var holeNumber = x
      if(playerNumber == 2) holeNumber += 7
      if(board.getStonesNumber(holeNumber) > 0) return holeNumber
    }
    -1
  }

}
