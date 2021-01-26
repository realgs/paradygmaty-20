import akka.actor.{Actor, ActorRef, PoisonPill, Props}

import scala.collection.mutable

class GameServer(val initial_stones: Int, val first_player: GameServer.PlayerData, val second_player: GameServer.PlayerData) extends Actor {

  private val first_pit = first_player.pits.head
  private val last_pit = second_player.kalah

  private val board = new mutable.HashMap[Int, Int]()
  initializeBoard()
  println("Initial board state:")
  printState()

  private var current_player = first_player
  private var opposite_player = second_player

  override def receive: Receive = {
    case GameServer.Start => current_player.actor ! Player.MoveRequest(board.toMap)
    case GameServer.MoveReply(pit) => playerMove(pit)
    case _ => println("Unknown command!")
  }

  def printState(): Unit = {
    print('\t')
    for (i <- second_player.pits.reverse) {
      printf("%2d ", board(i))
    }

    print("\n" + board(second_player.kalah) + "\t" + "  " * (second_player.pits.last / 2) + "\t\t" + board(first_player.kalah) + "\n\t")

    for (i <- first_player.pits) {
      printf("%2d ", board(i))
    }

    println('\n')

  }

  def playerMove(pit: Int): Unit = {

    if (!current_player.pits.contains(pit)) {
      println("It's not your turn or wrong pit index provided!")
      changePlayer()
      current_player.actor ! Player.MoveRequest(board.toMap)
      return
    }

    if(board(pit) == 0) {
      println("You can't move from an empty pit!")
      changePlayer()
      current_player.actor ! Player.MoveRequest(board.toMap)
      return
    }

    var stones = board(pit)
    board(pit) = 0

    var curr_pit = pit
    while (stones > 0) {
      curr_pit += 1
      if (curr_pit > last_pit) curr_pit = first_pit

      if (curr_pit != opposite_player.kalah) {
        board(curr_pit) += 1
        stones -= 1
      }
    }

    if (board(curr_pit) == 1 && current_player.pits.contains(curr_pit)) {
      // Ended on our empty pit
      board(current_player.kalah) += board(last_pit - curr_pit)
      board(last_pit - curr_pit) = 0
    }

    if (gameEnded()) {
      // Finished with empty pits
      moveStonesToKalah()
      declareWinner()

      context.stop(first_player.actor)
      context.stop(second_player.actor)
      context.stop(self)
      return
    }

    if (curr_pit != current_player.kalah) {
      // If we didn't end on player's kalah
      changePlayer()
    }

    printState()
    current_player.actor ! Player.MoveRequest(board.toMap)

  }

  def initializeBoard(): Unit = {
    for (i <- first_pit to last_pit + 1)
      board(i) = if (i != first_player.kalah && i != second_player.kalah) initial_stones else 0
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
    for (pit_index <- opposite_player.pits) {
      sum += board(pit_index)
      board(pit_index) = 0
    }
    board(opposite_player.kalah) += sum
  }

  def declareWinner(): Unit = {
    val first_player_stones = board(first_player.kalah)
    val second_player_stones = board(second_player.kalah)

    if (first_player_stones > second_player_stones) println("First player won with " + first_player_stones + " stones against " + second_player_stones + " stones")
    else if (second_player_stones > first_player_stones) println("Second player won with " + second_player_stones + " stones against " + first_player_stones + " stones")
    else println("Draw with " + first_player_stones + " stones")
  }

}

object GameServer {
  case class PlayerData(kalah: Int, pits: List[Int], actor: ActorRef)
  def props(initial_stones: Int, first_player: GameServer.PlayerData, second_player: GameServer.PlayerData): Props =
    Props(new GameServer(initial_stones, first_player, second_player))

  case class Start()
  case class MoveReply(pit: Int)
}
