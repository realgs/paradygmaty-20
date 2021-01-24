import scala.collection.mutable

class GameEngine(private val settings: GameSettings, private val players: (Player, Player)) {

  private val board = new mutable.HashMap[Int, Int]()
  initializeBoard()
  private var current_player = players._1
  private var opposite_player = players._2

  def printState(): Unit = {
    print('\t')
    for (i <- (players._2.pits.head to players._2.pits.last).reverse) {
      print(board(i) + "  ")
    }

    print("\n" + board(players._2.kalah_index) + "\t" + " " * settings.last_pit_index + "\t\t" + board(players._1.kalah_index) + "\n\t")

    for (i <- players._1.pits.head to players._1.pits.last) {
      print(board(i) + "  ")
    }

    println('\n')

  }

  def playerMove(): GameStatus.Value = {
    var pit = current_player.choosePit

    if (!current_player.pits.contains(pit)) {
      println("It's not your turn or wrong pit index provided!")
      return GameStatus.ERROR
    }

    var stones = board(pit)
    board(pit) = 0

    while (stones > 0) {
      pit += 1
      if(pit > settings.last_pit_index) pit = settings.first_pit_index

      if (pit != opposite_player.kalah_index) {
        board(pit) += 1
        stones -= 1
      }
    }

    if (board(pit) == 1 && current_player.pits.contains(pit)) {
      // Ended on our empty pit
      board(current_player.kalah_index) += board(settings.last_pit_index - pit)
      board(settings.last_pit_index - pit) = 0
    }

    if (pit != current_player.kalah_index) {
      // If we didn't end on player's kalah
      changePlayer()
    }

    if (gameEnded()) {
      moveStonesToKalah()
      declareWinner()
    } else {
      GameStatus.IN_PROGRESS
    }

  }

  def initializeBoard(): Unit = {
    for (i <- settings.first_pit_index to settings.last_pit_index)
      board(i) = if (i != players._1.kalah_index && i != players._2.kalah_index) settings.initial_stones else 0
  }

  def changePlayer(): Unit = {
    val tmp = current_player
    current_player = opposite_player
    opposite_player = tmp
  }

  def gameEnded(): Boolean = {
    for (pit_index <- current_player.pits)
      if (board(pit_index) != 0) return false
    true
  }

  def moveStonesToKalah(): Unit = {
    var sum = 0
    for (pit_index <- current_player.pits)
      sum += board(pit_index)
    board(current_player.kalah_index) += sum
  }

  def declareWinner(): GameStatus.Value = {
    val first_player_stones = board(players._1.kalah_index)
    val second_player_stones = board(players._2.kalah_index)
    if (first_player_stones > second_player_stones) {
      println("First player won with " + first_player_stones + " stones against " + second_player_stones + " stones")
      GameStatus.FIRST_PLAYER_WIN
    } else if (second_player_stones > first_player_stones) {
      println("Second player won with " + second_player_stones + " stones against " + first_player_stones + " stones")
      GameStatus.SECOND_PLAYER_WIN
    } else {
      println("Draw with " + first_player_stones + " stones")
      GameStatus.DRAW
    }
  }


}
