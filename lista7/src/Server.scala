import scala.annotation.tailrec
import scala.io.StdIn.readInt
import scala.util.control.Breaks.{break, breakable}

class Server(private val logic: Logic = new Logic()) {

  def start(): Unit = {
    val (player1, player2) = chooseMode()
    logic.print()

    breakable {
      while (!gameEnded()) {
        makeMove(player1, 0)
        logic.print()
        if (gameEnded()) break()
        makeMove(player2, 1)
        logic.print()
      }

    }

    addPoints()
    println("******FINAL BOARD*****")
    logic.print()
    val points1 = logic.getPlayer1Points
    val points2 = logic.getPlayer2Points
    val won = if (points1 > points2) 1 else 2

    if (logic.getPlayer1Points != logic.getPlayer2Points) println("Player" + won + " won with " + Math.max(points1, points2) + " points. Congratulations!!!!!!!!")
    else println("Draw! Nobody won :o")
  }

  private def makeMove(player: Engine, playerNum: Int): Unit = {
    var position = player.move(playerNum)
    if (wrongMove(position, player, playerNum)) return
    var results = logic.makeMove(position, playerNum)
    var successful = results._1
    var anotherMove = results._2
    while ((!successful || anotherMove) && !gameEnded()) {
      while (!successful) {
        println("Illegal pit - pit cannot be empty. Choose another one")
        position = player.move(playerNum)
        if (wrongMove(position, player, playerNum)) return
        results = logic.makeMove(position, playerNum)
        successful = results._1
        anotherMove = results._2
      }

      while (anotherMove && !gameEnded()) {
        logic.print()
        position = player.move(playerNum)
        if (wrongMove(position, player, playerNum)) return
        results = logic.makeMove(position, playerNum)
        successful = results._1
        anotherMove = results._2
      }
    }
  }

  private def wrongMove(position: Int, player: Engine, playerNum: Int): Boolean = {
    if (position == 0) true
    else if (position == -1) {
      makeMove(player, playerNum)
      true
    }
    else false
  }

  private def gameEnded(): Boolean = {
    var ifEnded1 = true
    var ifEnded2 = true
    for (i <- 0 until 6) {
      if (logic.getBoard.getPits1(i).get() != 0) ifEnded1 = false
      if (logic.getBoard.getPits2(i).get() != 0) ifEnded2 = false
    }
    ifEnded1 || ifEnded2
  }

  private def addPoints(): Unit = {
    for (i <- 0 until 6) {
      logic.getBoard.getMancalas(0).addAll(logic.getBoard.getPits1(i).take())
      logic.getBoard.getMancalas(1).addAll(logic.getBoard.getPits2(i).take())
    }
  }

  private def chooseMode(): (Engine, Engine) = {
    println("Choose mode:\n1 - computer vs computer\n2 - player vs computer\n3 - player vs player")

    @tailrec
    def chooseModeHelper(): (Engine, Engine) = {
      val choice: Int = readInt
      choice match {
        case 1 =>
          (new Computer, new Computer)
        case 2 =>
          (new Player(), new Computer)
        case 3 =>
          (new Player(), new Player())
        case _ =>
          println("Wrong value. Try once again")
          chooseModeHelper()
      }
    }

    chooseModeHelper()
  }
}
