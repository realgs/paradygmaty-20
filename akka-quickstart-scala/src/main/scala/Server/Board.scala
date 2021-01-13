package Server

import scala.annotation.tailrec

class Board(arrayBoard: Array[Int] = Array.empty[Int]) {

  var board: Array[Int] = arrayBoard
  if(board.length == 0)
    board = makeBoard()

  var ifFirstPlayerRound: Boolean = true //first player always starts
  var currentPlayerLastStonePosition: Int = -1


  private def makeBoard(): Array[Int] = {
    val stonesAmount = 6
    def innerHelper(index: Int): List[Int] = {
      if (index == 14) Nil
      else if ((index + 1) % 7 == 0) 0 :: innerHelper(index + 1)
      else stonesAmount :: innerHelper(index + 1)
    }
    innerHelper(0).toArray
  }


  def isGameFinished: Boolean = {
    if(board.slice(0, 6).sum == 0 && ifFirstPlayerRound)
      true
    else if(board.slice(7, 13).sum == 0 && !ifFirstPlayerRound)
      true
    else
      false
  }


  def ifChosenFieldIsCorrect(chosenField: Int): Boolean = {
    if(ifFirstPlayerRound) {
      if(chosenField < 0 || chosenField > 5)
        false
      else if(board(chosenField) == 0) // when field is empty
        false
      else
        true
    } else {
      if(chosenField < 7 || chosenField > 12)
        false
      else if(board(chosenField) == 0)
        false
      else
        true
    }
  }


  def makeMove(chosenField: Int): Unit = {
    val stonesFromField = board(chosenField)
    board(chosenField) = 0

    @tailrec
    def innerMove(index: Int, stones: Int): Unit = {
      if(stones > 1) {
        board(index) += 1
        val nextIndex = {
          if (index == 13)
            0
          else
            index + 1
        }
        innerMove(nextIndex, stones - 1)
      } else { // for the last stone we also change currentPlayerLastStonePosition field - useful for calculating who is next
        board(index) += 1
        currentPlayerLastStonePosition = index
      }
    }

    innerMove(chosenField + 1, stonesFromField)
  }


  def takeEnemyStonesIfPossible(): Unit = {
    if(ifFirstPlayerRound) {
      if(currentPlayerLastStonePosition >= 0 && currentPlayerLastStonePosition <= 5 && board(currentPlayerLastStonePosition) == 1) { // last stone was put into empty field
        val oppositeField = 12 - currentPlayerLastStonePosition
        board(6) += board(oppositeField)
        board(oppositeField) = 0
      }
    } else {
      if(currentPlayerLastStonePosition >= 7 && currentPlayerLastStonePosition <= 12 && board(currentPlayerLastStonePosition) == 1) { // last stone was put into empty field
        val oppositeField = 12 - currentPlayerLastStonePosition
        board(13) += board(oppositeField)
        board(oppositeField) = 0
      }
    }
  }


  def nextPlayer(): Unit = {
   if(ifFirstPlayerRound) { //this round was for 1 player
     if(currentPlayerLastStonePosition != 6) {
       ifFirstPlayerRound = false
     }
     currentPlayerLastStonePosition = -1 //set to init value
   } else {
     if(currentPlayerLastStonePosition != 13) {
       ifFirstPlayerRound = true
     }
     currentPlayerLastStonePosition = -1
   }
  }


  private def calculateResult(): (Int, Int) = {
    for(i <- 0 to 5) {
      board(6) += board(i)
    }
    for(i <- 7 to 12) {
      board(13) += board(i)
    }
    (board(6), board(13))
  }


  def showResults(): Unit = {
    val (p1Result, p2Result) = calculateResult()
    println("Player 1: " + p1Result + " stones in base")
    println("Player 2: " + p2Result + " stones in base" )
    if(p1Result > p2Result)
      println("Player 1 won!")
    else if(p2Result > p1Result)
      println("Player 2 won!")
    else
      println("Draw!")
  }


  def printBoard(): Unit = {
    if(ifFirstPlayerRound) {
      println("")
      println("----------------Player 1 Round--------------------")
      println(s"P2          [${board(12)}] - [${board(11)}] - [${board(10)}] - [${board(9)}] - [${board(8)}] - [${board(7)}]")
      println(s"       [${board(13)}]                                    [${board(6)}]")
      println(s"P1          [${board(0)}] - [${board(1)}] - [${board(2)}] - [${board(3)}] - [${board(4)}] - [${board(5)}]")
      println("Fields nr:   0     1     2     3     4     5")

      /*----------------Player 1 Round--------------------
      P2          [6] - [6] - [6] - [6] - [6] - [6]
             [0]                                     [0]
      P1          [6] - [6] - [6] - [6] - [6] - [6]
      Fields nr:   0     1     2     3     4     5*/
    } else {
      println("")
      println("----------------Player 2 Round--------------------")
      println(s"P1          [${board(5)}] - [${board(4)}] - [${board(3)}] - [${board(2)}] - [${board(1)}] - [${board(0)}]")
      println(s"       [${board(6)}]                                    [${board(13)}]")
      println(s"P2          [${board(7)}] - [${board(8)}] - [${board(9)}] - [${board(10)}] - [${board(11)}] - [${board(12)}]")
      println("Fields nr:   7     8     9    10     11    12")

      /*----------------Player 2 Round--------------------
      P1          [6] - [6] - [6] - [6] - [6] - [6]
             [0]                                     [0]
      P2          [6] - [6] - [6] - [6] - [6] - [6]
      Fields nr:   7     8     9    10     11    12*/
    }

  }
}
