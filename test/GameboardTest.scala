package pl.pawelklecha.kalaha

import players.Player
import server.Gameboard

import org.scalatest.FunSuite

class GameboardTest extends FunSuite {

  test("simpleMove") {
    val gameboard = new Gameboard()

    gameboard.makeMove(0)

    assert(gameboard.getGameboard sameElements Array(0, 5, 5, 5, 5, 4, 0, 4, 4, 4, 4, 4, 4, 0))
  }

  test("checkResult") {
    val gameboard = new Gameboard()
    val gameboardArray = gameboard.getGameboard

    for (i <- Gameboard.PLAYER1_FIRST_INDEX until Gameboard.PLAYER1_BASE_INDEX) gameboardArray(i) = 0

    gameboard.printResult()

    assert(gameboardArray(Gameboard.PLAYER1_BASE_INDEX) == 0 && gameboardArray(Gameboard.PLAYER2_BASE_INDEX) == 24)
  }

  test("takeOpponentStones") {
    val gameboard = new Gameboard()
    val gameboardArray = gameboard.getGameboard

    gameboardArray(1) = 1
    gameboardArray(2) = 0
    gameboardArray(10) = 4

    gameboard.makeMove(1)

    assert(gameboardArray(Gameboard.PLAYER1_BASE_INDEX) == 5)
  }

  test("freeMoveAvailability") {
    val gameboard = new Gameboard()
    val gameboardArray = gameboard.getGameboard

    gameboardArray(5) = 1
    gameboard.makeMove(5)

    assert(gameboard.getCurrentPlayer == Player.PLAYER_1)
  }

  test("invalidMove") {
    val gameboard = new Gameboard()

    gameboard.getGameboard(0) = 0

    assert(!gameboard.checkMoveCorrectness(0))
    assert(!gameboard.checkMoveCorrectness(-1))
    assert(!gameboard.checkMoveCorrectness(10))
    assert(gameboard.checkMoveCorrectness(1))

    gameboard.makeMove(1)

    assert(!gameboard.checkMoveCorrectness(1))
  }
}
