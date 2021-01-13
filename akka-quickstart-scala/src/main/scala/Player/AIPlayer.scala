package Player

import Server._
import akka.actor.Actor

class AIPlayer(private val ifFirstPlayer: Boolean) extends Actor {

  override def receive: Receive = {
    case Server.ChooseField(board) =>
      var chosenField = -1
      var stonesInOpponentsHole = -1
      var lowerHole = 0
      var upperHole = 5
      if(!ifFirstPlayer) {
        lowerHole += 7
        upperHole += 7
      }

      //checks if there is a move that will let us take opponent's stones - our last stone lands in our empty field, and opponent's hole is not empty
      for(x <- lowerHole to upperHole) {
        if(board.board(x) != 0) {
          val nextMoveBoard = new Board(board.board.clone())
          if(!ifFirstPlayer)
            nextMoveBoard.ifFirstPlayerRound = false

          nextMoveBoard.makeMove(x)

          if(ifFirstPlayer) {
            if(nextMoveBoard.currentPlayerLastStonePosition >= 0 && nextMoveBoard.currentPlayerLastStonePosition <= 5 && nextMoveBoard.board(nextMoveBoard.currentPlayerLastStonePosition) == 1) { // in last hole where we put our stone must be 1 stone now, it means it was empty before the move
              val oppositeField = 12 - nextMoveBoard.currentPlayerLastStonePosition
              if(nextMoveBoard.board(oppositeField) > 0 && nextMoveBoard.board(oppositeField) > stonesInOpponentsHole) { //we want to make this move if oppositeField is not empty and will give us more stones than other moves like that (if any)
                chosenField = x
                stonesInOpponentsHole = nextMoveBoard.board(oppositeField)
              }
            }
          } else {
            if(nextMoveBoard.currentPlayerLastStonePosition >= 7 && nextMoveBoard.currentPlayerLastStonePosition <= 12 && nextMoveBoard.board(nextMoveBoard.currentPlayerLastStonePosition) == 1){
              val oppositeField = 12 - nextMoveBoard.currentPlayerLastStonePosition
              if(nextMoveBoard.board(oppositeField) > 0 && nextMoveBoard.board(oppositeField) > stonesInOpponentsHole) {
                chosenField = x
                stonesInOpponentsHole = nextMoveBoard.board(oppositeField)
              }
            }
          }
        }
      }

      if(chosenField != -1){ // if field wasn't chosen yet
        //checks if there is a move that will give us another round - our last stone lands in our base
        for(x <- lowerHole to upperHole) {
          if(board.board(x) != 0) {
            val nextMoveBoard = new Board(board.board.clone())
            if(!ifFirstPlayer)
              nextMoveBoard.ifFirstPlayerRound = false

            nextMoveBoard.makeMove(x)

            if(ifFirstPlayer) {
              if(nextMoveBoard.currentPlayerLastStonePosition == 6)
                chosenField = x
            } else {
              if(nextMoveBoard.currentPlayerLastStonePosition == 13)
                chosenField = x
            }
          }
        }
      }

      if(chosenField == -1) { // if field wasn't chosen yet
        //choose pit with the biggest amount of stones - we want ot avoid takeover of stones in that pit
        var currentBiggestAmount = -1
        for(x <- lowerHole to upperHole) {
          if(board.board(x) != 0 && board.board(x) > currentBiggestAmount) {
            currentBiggestAmount = board.board(x)
            chosenField = x
          }
        }
      }

      print("Computer choose: " + chosenField)
      sender ! Server.ConfirmMyMove(chosenField)
  }

}
