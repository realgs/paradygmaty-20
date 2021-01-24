package scala

import scala.util.Random

abstract class PlayerNumber {
  val holesNumbers: Array[Int]
}

object PlayerOne extends PlayerNumber {
  val holesNumbers: Array[Int] = Array(1, 2, 3, 4, 5, 6, 7)
}

object PlayerTwo extends PlayerNumber {
  val holesNumbers: Array[Int] = Array(8, 9, 10, 11, 12, 13, 14)
}

abstract class Player(val playerNumber: PlayerNumber) {
  val holesNumbers: Array[Int] = playerNumber.holesNumbers

  def receiveNumber: Int

  def getPlayerNumber: Int = playerNumber match {
    case PlayerOne => 1
    case PlayerTwo => 2
  }

}

class NPCPlayer(playerNumber: PlayerNumber) extends Player(playerNumber) {
  override def receiveNumber: Int = Random.between(1, 7)
}

class IntelligentNPCPlayer(playerNumber: PlayerNumber, board: Board) extends Player(playerNumber) {
  override def receiveNumber: Int = playerNumber match {
    case PlayerOne => board.selectHoleWithTheSmallestAmountOFStones(PlayerOne)
    case PlayerTwo => board.selectHoleWithTheSmallestAmountOFStones(PlayerTwo)
  }
}

class HumanPlayer(playerNumber: PlayerNumber) extends Player(playerNumber) {
  override def receiveNumber: Int = {
    Helper.getInputFromUser
  }
}
