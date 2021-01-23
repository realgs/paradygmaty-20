package L7_main
import scala.util.control.Breaks._

object KalahaAI {
  def chooseMove(b: Board, depth: Int, playerNumber: Int): Int={
    decisionMinMaxTree(b, depth, true, true, playerNumber)._2
  }

  def decisionMinMaxTree(b: Board, depth: Int, myTurn: Boolean, firstIter: Boolean = false, playerNumber: Int = 1, _alpha: Int = Integer.MIN_VALUE, _beta: Int = Integer.MAX_VALUE): (Int, Int) ={
    if(depth == 0 || b.is_game_over()._1){ // if this is last depth level => returning current bases difference
      if(playerNumber == 1)
        return (b.board_array(0) - b.board_array(7), 0)
      else
        return (b.board_array(7) - b.board_array(0), 0)
    }
    var bestMoveNumber = -1

    val board_copy = b.clone()
    // both player starts with their worst possible score
    var alpha = _alpha // alpha stands for minimal score which player is assured of
    var beta = _beta // beta stands for maximal score which enemy is assured of
    breakable {
      if (myTurn) { // searching for the highest diffrence between two players bases
        var bestValue = Integer.MIN_VALUE

        for (i <- 1 to 6) { // testing all possible moves
          var value = 0
          if (board_copy.is_legal_move(i)) { // checking if move is possible (marbles in hole != 0)
            if (board_copy.move_repeat(i)) { // checking for repeated move on copied board
              val board = b.clone() // making next copy to be used in deeper iteration
              board.make_move(i)
              value = decisionMinMaxTree(board, depth, true, false, playerNumber, alpha, beta)._1
            }
            else { // next move opponent
              val board = b.clone()
              board.make_move(i)
              value = decisionMinMaxTree(board, depth - 1, false, false, playerNumber, alpha, beta)._1
            }

            if (firstIter && value > bestValue) { // every time better value is found => update of best move <- i
              bestMoveNumber = i
            }
            bestValue = value.max(bestValue)
            alpha = alpha.max(bestValue) // if better move was found, updating alpha to new assured score value
            if (alpha >= beta) { // if enemy max score is less than player min score, we dont have to search for result, becouse its the best
              break
            }
          }
        }
        return (bestValue, bestMoveNumber)
      }
      else { // searching for the lowest diffrence between two players bases
        var bestValue = Integer.MAX_VALUE

        for (i <- 1 to 6) {
          var value = 0
          if (board_copy.is_legal_move(i)) {
            if (board_copy.move_repeat(i)) {
              val board = b.clone()
              board.make_move(i)
              value = decisionMinMaxTree(board, depth, false, false, playerNumber, alpha, beta)._1
            }
            else {
              val board = b.clone()
              board.make_move(i)
              value = decisionMinMaxTree(board, depth - 1, true, false, playerNumber, alpha, beta)._1
            }
            bestValue = value.min(bestValue)
            beta = beta.min(bestValue)
            if (alpha >= beta) { // if enemy max score is less than player min score, we dont have to search for result, becouse its the best
              break
            }
          }
        }
        return (bestValue, bestMoveNumber)
      }
    }
    (0, 0) // never used
  }
}
