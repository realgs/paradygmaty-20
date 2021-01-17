package gameboard

import model.GameConstants._
import model.Player

class BoardPrinter(board: Array[Int]) {
  def printBoard(player: Player.Value): Unit = {
    player match {
      case Player.First => printFirstPlayerPerspective()
      case Player.Second => printSecondPlayerPerspective()
    }

    print("\n\n")
  }

  private def printFirstPlayerPerspective(): Unit = {
    println("PLAYER ONE TURN")

    print("   ")

    for (i <- (PLAYER_TWO_BASE_INDEX - 1 to PLAYER_TWO_FIRST_HOLE_INDEX) by -1) {
      print(s"${board(i)} ")
    }

    print("  \n")

    print(s"${board(PLAYER_TWO_BASE_INDEX)}  ")

    for (_ <- 0 to PLAYERS_HOLES_NUMBER * 2) {
      print(" ")
    }

    print(s"${board(PLAYER_ONE_BASE_INDEX)}\n")

    print("   ")


    for (i <- PLAYER_ONE_FIRST_HOLE_INDEX until PLAYER_ONE_BASE_INDEX) {
      print(s"${board(i)} ")
    }

    print("  \n")
  }


  private def printSecondPlayerPerspective(): Unit = {
    println("PLAYER TWO TURN")

    print("   ")

    for (i <- (PLAYER_ONE_BASE_INDEX - 1 to PLAYER_ONE_FIRST_HOLE_INDEX) by -1) {
      print(s"${board(i)} ")
    }

    print("  \n")

    print(s"${board(PLAYER_ONE_BASE_INDEX)}  ")

    for (_ <- 0 to PLAYERS_HOLES_NUMBER * 2) {
      print(" ")
    }

    print(s"${board(PLAYER_TWO_BASE_INDEX)}\n")

    print("   ")

    for (i <- PLAYER_TWO_FIRST_HOLE_INDEX until PLAYER_TWO_BASE_INDEX) {
      print(s"${board(i)} ")
    }

    print("  \n")
  }
}

object BoardPrinter {
  def apply(board: Array[Int]): BoardPrinter = new BoardPrinter(board)
}
