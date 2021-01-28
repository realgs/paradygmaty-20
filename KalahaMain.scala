import scala.io.StdIn
import scala.concurrent.{Await, Future}
import java.util.Timer
import java.util.concurrent.TimeoutException
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{DurationInt, FiniteDuration}

class KalahaMain(val field: GameField, val side: Int){

    def endGame():Boolean ={
      if(field.player1.forall(i => i==0) || field.player2.forall(i => i==0)) true;
      else false;
    }

    def PlayersPoints: (Int, Int) = {
      val p1Score = field.kalahaCell1 + field.player1.sum
      val p2Score = field.kalahaCell2 + field.player2.sum
      (p1Score, p2Score)
    }

    def playSide(position: Int): KalahaMain = {
      val (lastSide, lastPos) = field.lastPosition(side, position)
      val newBoard = field.move(side, position)
      val nextTurn = if (lastPos == 6) side else if (side == 1) 2 else 1
      val (playerSide, oppSide) =
        if (side == 1) (newBoard.player1, newBoard.player2)
        else (newBoard.player2, newBoard.player1)

      val finalBoard =
        if (
          lastPos != 6 &&
            lastSide == side &&
            playerSide(lastPos) == 1 &&
            oppSide(lastPos) != 0) newBoard.capture(side, lastPos)
        else newBoard
      new KalahaMain(finalBoard, nextTurn)
    }
  }

object KalahaMain {
  var timeGame = true;
  var whoPlays1 = 0
  def main(args: Array[String]) = {
    val welcomeMessage =
      """
        |Start
        |""".stripMargin
    println(welcomeMessage)
    var game = KalahaMain()
    val (player1, player2) = getPlayers

    //main loop
    while (!game.endGame() && timeGame) {
      println("\n" + "=" * 50)
      // pozycja biezacego gracza
      if (game.side == 1) {
        println(game.field)
        printBoardPositions
      } else {
        printBoardPositions
        println(game.field)
      }

      print(s"Player ${game.side}'s turn: ")
      val time1 = System.nanoTime() / 1000000000
      val pos: Int =
        if (game.side == 1) player1.getMove(game.field, 1)
        else player2.getMove(game.field, 2)
        game = game.playSide(pos)
      whoPlays1 = pos;
      val time2 = System.nanoTime() / 1000000000
      val timeRes = time2 - time1;

      if(timeRes > 10){
        timeGame = false
      }else{
        timeGame = true;
      }
    }

    // Print final score
    println(game.field)
    if(timeGame == true) {
      val (p1score, p2score) = game.PlayersPoints
      println(s"Gracz 1: $p1score   Gracz 2: $p2score")
      val zwycięzca = if (p1score > p2score) 1 else 2
      println(s"Player $zwycięzca zwycięzca!")
    }
    else{
      if(whoPlays1 == 1) println("Time is out!"  + "\n" + "Gracz 2 zwycięzca!")
      else println("Time is out!"  + "\n" + "Gracz 1 zwycięzca!")
    }
  }

  //nazwa komorek
  def printBoardPositions() = {
    println("       (a)   (b)   (c)   (d)   (e)   (f)")
  }

  def getPlayers: (Player, Player) = {
    def getPlayer(playerNum: Int): Player = {
      val options =
        """1) Czlowiek
           #2) Komputer
        """.stripMargin('#')
      println(s"Wybierz gracza $playerNum?")
      println(options)
      var selection = StdIn.readLine
      while (selection.length > 1 || (selection(0) > '2' && selection(0) < '1')) {
        println("Nie poprawny numer. Wybierz 1 czy 2")
      }
      if (selection(0) == '1'){
        new Human
      }
      else{
        new Computer
      }
    }
    (getPlayer(1), getPlayer(2))
  }

  //Nowa gra
  def apply() = new KalahaMain(GameField(), 1)
}
