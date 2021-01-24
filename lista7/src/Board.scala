import Board.{mancalas, pits1, pits2}
//pits indexes: 1-6 player1, 7-12 player2
//mancalas indexes: 1 - player1, 2 - player2

//TODO: how to write this class? def in object or in class
class Board(pits1: Array[Pit] = pits1, pits2: Array[Pit] = pits2, mancalas: Array[Mancala] = mancalas) {
  def print(): Unit = {
    var m1 = ""
    var m2 = ""
    var st2 = ""
    var st3 = ""
    if (mancalas(1).get() > 9) {
      m1 = "–"
      st2 = " "
    }
    if (mancalas(0).get() > 9) {
      m2 = "–"
      st3 = " "
    }


    println(s"╭–––$m1–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––$m2––––––╮")
    println(s"|  ╭–$m1––╮    ╭–––╮   ╭–––╮   ╭–––╮   ╭–––╮   ╭–––╮   ╭–––╮     ╭–$m2––╮  |")
    println(s"|  |  $st2 |    |${pits2(5)} |   |${pits2(4)} |   |${pits2(3)} |   |${pits2(2)} |   |${pits2(1)} |   |${pits2(0)} |     | $st3  |  |")
    println(s"|  |  $st2 |    ╰–––╯   ╰–––╯   ╰–––╯   ╰–––╯   ╰–––╯   ╰–––╯     |  $st3 |  |")
    println(s"|  | ${mancalas(1)} |                                                      | ${mancalas(0)} |  |")
    println(s"|  |  $st2 |    ╭–––╮   ╭–––╮   ╭–––╮   ╭–––╮   ╭–––╮   ╭–––╮     |  $st3 |  |")
    println(s"|  |  $st2 |    |${pits1(0)} |   |${pits1(1)} |   |${pits1(2)} |   |${pits1(3)} |   |${pits1(4)} |   |${pits1(5)} |     |  $st3 |  |")
    println(s"|  ╰–$m1––╯    ╰–––╯   ╰–––╯   ╰–––╯   ╰–––╯   ╰–––╯   ╰–––╯     ╰–$m2––╯  |")
    println(s"╰––––$m1–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––$m2–––––╯")
  }

  def copy(): Board = {
    val newPits1 = new Array[Pit](6)
    val newPits2 = new Array[Pit](6)
    val newMancalas = new Array[Mancala](2)
    for (i <- 0 until 6) {
      newPits1(i) = pits1(i).copy()
      newPits2(i) = pits2(i).copy()
    }
    newMancalas(0) = mancalas(0).copy()
    newMancalas(1) = mancalas(1).copy()
    new Board(newPits1, newPits2, newMancalas)
  }

  def getPits(player: Int): Array[Pit] = {
    player match {
      case 0 => pits1
      case 1 => pits2
      case _ => throw new IllegalArgumentException("getPits")
    }
  }

  def getPits1: Array[Pit] = {
    pits1
  }

  def getPits2: Array[Pit] = {
    pits2
  }

  def getMancalas: Array[Mancala] = {
    mancalas
  }

  def getMancala(number: Int): Mancala = {
    mancalas(number)
  }

}

object Board {
  val pits1: Array[Pit] = Array.fill(6)(new Pit())
  val pits2: Array[Pit] = Array.fill(6)(new Pit())
  val mancalas: Array[Mancala] = Array.fill(2)(new Mancala())
}
