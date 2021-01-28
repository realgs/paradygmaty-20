import scala.io.StdIn
import scala.util.Random

trait Player {
    //turn = player1 or playre2
    def getMove(field: GameField, turn: Int): Int
    var ending = true;
  }

  //Gracz - czlowiek
  class Human extends Player {

    def getMove(field: GameField, side: Int): Int = {

      var position: Int = StdIn.readChar - 'a'

      while (position < 0 || position > 5) {
        println("Nie poprawna pozycja")
        position = StdIn.readChar - 'a'
      }
      position
    }

    def endGameOfTime(): Boolean ={
      ending = false;

      ending
    }
  }

  //Gracz komputer (random moves)
  class Computer extends Player {

    def getMove(field: GameField, turn: Int): Int = {
    val side = if (turn == 1) field.player1 else field.player2
    val valid_moves = side.zipWithIndex.filter { case (value, index) => value > 0 }.map(_._2)
    val selected_move = valid_moves(Random.nextInt(valid_moves.length))
    Thread sleep 1000  //1 sec wait
    println(('a' + selected_move).toChar)
      selected_move
    }

    def endGameOfTime(): Boolean ={
      true
    }

  }
