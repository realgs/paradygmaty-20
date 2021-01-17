//1) Jeżeli użytkownik nie poda tablicy to zostania wykorzystana domyślna z 4 kamieniami
//2) player_turn -> true zaczyna gracz pierwszy, false zaczyna gracz drugi
class Game(private var playerOneStart:Boolean = true,private var stones :Int = 4)
{
  // Tworzy planszę, każdy z graczy posiada po 6 pól
  var board = Array(stones,stones,stones,stones,stones,stones,0,stones,stones,stones,stones,stones,stones,0)
  // Wygląd planszy do gry
  //    12 11 10 09 08 07
  // 13                   06
  //    00 01 02 03 04 05
  // Gracz pierwszy ma swoją bazę pod indeksem 6,a drugi pod indeksem 13
  private var isPlayerOneTurn = playerOneStart // Czy obecnie jest tura gracza pierwszego(True)
  private val totalNumberOfStones = board.sum


  def copyOfGame():Game =
  {
    val copyOfGame = Game(isPlayerOneTurn,stones)

    copyOfGame.board = board.clone()

    copyOfGame
  }

  def validMovesForPlayer():Array[Int] =
  {
     if(isPlayerOneTurn) validMovesForPlayerOne()
     else validMovesForPlayerTwo()
  }

  private def validMovesForPlayerOne():Array[Int] =
  {
     val list = List(0,1,2,3,4,5)
     list.filter(index => board(index) != 0).toArray
  }

  private def validMovesForPlayerTwo():Array[Int] =
  {
    val list = List(7,8,9,10,11,12)
    list.filter(index => board(index) != 0).toArray
  }

  private def getGameBoard():Array[Int] = board
  def getIndexOfTurnPlayer():Int = if(isPlayerOneTurn) 1 else 2
  private def getActualScore():(Int,Int) = (board(6),board(13))
  def isGameOver():Boolean =
    {
        isOneSideEmpty() || getActualScore()._1 > totalNumberOfStones / 2 ||
        getActualScore()._2 > totalNumberOfStones / 2
    }

  //Zwróć zero jeżeli gra zakończyła się remisem
  def getWinner():Int =
  {
    if(isOneSideEmpty())
    {
      board(6) += board.slice(0,6).sum
      board(13) += board.slice(7,13).sum
      for(i <- 0 to 5) board(i) = 0
      for(i <- 7 to 12) board(i) = 0
    }
    if(leadOfPlayerOne() == leadOfPlayerTwo()) 0
    else if(leadOfPlayerOne() > leadOfPlayerTwo()) 1 else 2
  }
  def leadOfPlayer():Int =
  {
    if(isPlayerOneTurn) leadOfPlayerOne()
    else leadOfPlayerTwo()
  }
  private def leadOfPlayerOne():Int =
  {
    var add = 0

    if(isOneSideEmpty())
    {
      add  = board.slice(0,6).sum
    }

    add + board(6) - board(13)
  }
  private def leadOfPlayerTwo():Int =
  {
    var add = 0

    if(isOneSideEmpty())
    {
      add  = board.slice(7,13).sum
    }

    board(13) - board(6)
  }

  // True jeżeli jeden z graczy nie ma możliwości ruchu
  private def isOneSideEmpty():Boolean =
  {
    val sumOfPlayerOneBoard = board.slice(0,6).sum
    val sumOfPlayerTwoBoard = board.slice(7,13).sum
    sumOfPlayerOneBoard == 0 || sumOfPlayerTwoBoard == 0
  }

  def displayBoard():Unit =
  {
    print(" " * 3)
    for(i <- 12 to 7 by -1)
    {
      print(convertIntToString(i) + " ")
    }
    println("\n"+convertIntToString(13) + " " * 19 + convertIntToString(6))
    print(" " * 3)
    for(i <- 0 to 5)
    {
      print(convertIntToString(i) + " ")
    }
    println("\n")

    print(" " * 3)
    for(i <- 12 to 7 by -1)
    {
      print(convertIntToString(board(i)) + " ")
    }
    println("\n"+convertIntToString(board(13)) + " " * 19 + convertIntToString(board(6)))
    print(" " * 3)
    for(i <- 0 to 5)
    {
      print(convertIntToString(board(i)) + " ")
    }
    println("\n")

  }
  private def convertIntToString(number:Int):String =
  {
    if(number < 10) "0" + number.toString
    else number.toString
  }

  private def doesPlayerOwnZone(numberOfZone:Int):Boolean =
  {
    if(isPlayerOneTurn) doesPlayerOneOwnZone(numberOfZone)
    else doesPlayerTwoOwnZone(numberOfZone)
  }
  private def doesPlayerOneOwnZone(numberOfZone:Int):Boolean =
  {
    numberOfZone >= 0 && numberOfZone <= 5
  }
  private def doesPlayerTwoOwnZone(numberOfZone:Int):Boolean =
  {
    numberOfZone >= 7 && numberOfZone <= 12
  }
  private def isZoneEmpty(numberOfZone:Int):Boolean = board(numberOfZone) == 0

  // True jeżeli ruch się udał, False jeżeli nie
  def move(numberOfZone:Int):Boolean =
  {
    if(!doesPlayerOwnZone(numberOfZone)) return false
    if(isZoneEmpty(numberOfZone)) return false

    var stonesInZone = board(numberOfZone)
    var currentZone = numberOfZone
    board(currentZone) = 0

    while(stonesInZone > 0)
    {
      currentZone = (currentZone + 1) % board.length
      // Zgodnie z zasadami moje kamyki nie trafiają do bazy przeciwnika
      if(isPlayerOneTurn && currentZone == 13) currentZone = 0
      if(!isPlayerOneTurn && currentZone == 6) currentZone = 7
      board(currentZone) += 1
      stonesInZone -= 1
    }

    // Koniec gry
    if(isOneSideEmpty())
    {
      board(6) += board.slice(0,6).sum
      board(13) += board.slice(7,13).sum
      for(i <- 0 to 5) board(i) = 0
      for(i <- 7 to 12) board(i) = 0
      return true
    }

    // Jeżeli ostatni kamyk wylądował w bazie to gracz ma jeszcze jedną turę
    if(currentZone == 6 || currentZone == 13) return true

    // Jeżeli ostatni kamyk wyląduje w pustym dołku i przeciwny dołek ma kamienie
    // to wszystkie kamienie trafiają do naszej bazy
    if(doesPlayerOwnZone(currentZone) && board(currentZone) == 1 && board(12 - currentZone) >= 1)
    {
      val extraStones = 1 + board(12 - currentZone)
      board(currentZone) = 0
      board(12 - currentZone) = 0
      if(isPlayerOneTurn) board(6) += extraStones
      else board(13) += extraStones
    }

    // Zmiana tury gracza
    isPlayerOneTurn = !isPlayerOneTurn

    true
  }
}

// Obiekt towarzyszący
object Game
{
  def apply(playerOneStart:Boolean = true,stones :Int = 4):Game =
  {
    new Game(playerOneStart,stones)
  }
}
