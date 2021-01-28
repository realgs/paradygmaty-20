

class GameField(val player1:Seq[Int], val kalahaCell1:Int, val player2:Seq[Int], val kalahaCell2:Int) {

  //tylko dwóch graczy, 1 i 2
  //position, sześć komórek dla 1 gracza, czyli od [0 do 5]
  def move(player: Int, position: Int): GameField = {
    if (player != 1 && player != 2) throw new IllegalArgumentException
    if (position < 0 || position > 5) throw new IllegalArgumentException

    new GameField(Seq(4, 7), 8, Seq(6, 5), 8) //tylko do testu!!!

    //distence - distence from one cells to another by stones
    //return stones after passing self side, and new progres for this player
    def playing(stones: Int, position: Int, side: Seq[Int]) = {
      val distance = math.min(stones, 6 - position)
      val putStoens = side.zipWithIndex.map {
        case (a, b) => if (b >= position && b < position + distance) a + 1 else a
      }
      (stones - distance, putStoens)
    }

    //komorka kalaha, to komorka gdzie znajduje sie bank gracza
    //put stones to the kalaha cell
    def inverseKalaha(stones: Int, kalaha: Int): (Int, Int) = {
      if (stones > 0) (stones - 1, kalaha + 1) else (stones, kalaha)
    }

    //zmieniamy strony moejscamy i odwracamy liste dla gracza #2, bo zapelniamy komórki od 0 indeksa
    val (sideOne, sideTwo) =
      if (player == 1) (player1.toArray, player2.reverse.toArray)
      else (player2.reverse.toArray, player1.toArray)

    val start = if (player == 1) position else 5 - position

    val playerKalah = if (player == 1) kalahaCell1 else kalahaCell2
    //    if (sideOne(start) == 0) throw new IllegalStateException

    val stones = sideOne(start)
    sideOne(start) = 0 //zmieniamy pojemnosc komorki na zero, bo wyciagamy wszystkie kamienie

    val posPlus1 = start + 1

    //Pierwsze przejscie
    val (seedsFirstPass, playerPodsFirstPass) = playing(stones, posPlus1, sideOne)
    val (seedsFirstPassAndBank, playerBankFirstPass) = inverseKalaha(seedsFirstPass, playerKalah)

    //Przejscie po drugiej stronie
    val (seedsSecondPass, oppSideSecondPass) = playing(seedsFirstPassAndBank, 0, sideTwo)

    //Nie umiesiamy kamien w komorce przewnika i kontynujemy
    val (seedsThirdPass, playerPodsThirdPass) = playing(seedsSecondPass, 0, playerPodsFirstPass)

    // stop
    val (seedsThirdsPassAndBank, playerBankSecondPass) = inverseKalaha(seedsThirdPass, playerBankFirstPass)
    val (seedsFourthPass, oppSideFourthPass) = playing(seedsThirdsPassAndBank, 0, oppSideSecondPass)

    //sprawdzic poprawnosc

    // New GameField (render)
    if (player == 1)
      new GameField(playerPodsThirdPass.toVector, playerBankSecondPass, oppSideFourthPass.reverse.toVector, kalahaCell2)
    else
      new GameField(oppSideFourthPass.toVector, kalahaCell1, playerPodsThirdPass.reverse.toVector, playerBankSecondPass)
  }


  def capture(player: Int, position: Int): GameField = {
    assert(player1(position) != 0 && player2(position) != 0)
    val (newKalah1, newKalah2) =
      if (player == 1)
        (kalahaCell1 + player1(position) + player2(position), kalahaCell2)
      else
        (kalahaCell1, kalahaCell2 + player1(position) + player2(position))
    new GameField(player1.updated(position, 0), newKalah1, player2.updated(position, 0), newKalah2)
  }

  //return (gracz, ostatnia pozycja gdzie umiszczono ostatni kamien
  def lastPosition(player: Int, position: Int): (Int, Int) = {
    val stones = (if (player == 1) player1 else player2) (position)
    val distanceKalah = if (player == 1) 6 - position else position + 1

    //dla wszystkich mozliwych przypadkow
    if (player == 1)
      if (stones < distanceKalah) (1, position + stones)
      else if (stones == distanceKalah) (1, 6)
      else if (stones <= distanceKalah + 6) (2, 6 - (stones - distanceKalah))
      else if (stones <= distanceKalah + 12) (1, stones - distanceKalah - 7)
      else if (stones == distanceKalah + 13) (1, 6)
      else if (stones <= distanceKalah + 19) (2, 19 - (stones - distanceKalah))
      else {
        assert(false); (0, 0)
      }
    else if (stones < distanceKalah) (2, position - stones)
    else if (stones == distanceKalah) (2, 6)
    else if (stones <= distanceKalah + 6) (1, stones - distanceKalah - 1)
    else if (stones <= distanceKalah + 12) (2, 12 - (stones - distanceKalah))
    else if (stones == distanceKalah + 13) (2, 6)
    else if (stones <= distanceKalah + 19) (1, stones - distanceKalah - 14)
    else {
      assert(false); (0, 0)
    }
  }

  override def toString = {
    val top = player2.foldLeft("")((a, b) => a + StringContext(" ", "%-3s| ").f(b)).trim
    val bottom = player1.foldLeft("")((a, b) => a + StringContext(" ", "%-3s| ").f(b)).trim
    f"""
       >${"-" * 50}
       >|    |  $top    |
       >| $kalahaCell2%-3s|${" " * 17}|${" " * 17}| $kalahaCell1%-3s|
       >|    |  $bottom    |
       >${"-" * 47}
       >""".stripMargin('>').trim
  }
}

  //Obiekt towarzyszacy
  object GameField {
    def apply() = {
      val creatCells = for (i <- 1 to 6) yield 6
      new GameField(creatCells, 0, creatCells, 0)
    }
  }
