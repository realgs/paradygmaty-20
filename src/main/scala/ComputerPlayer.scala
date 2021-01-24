import scala.collection.mutable
import scala.util.Random

class ComputerPlayer(id: Int, kalah_index: Int, pits: List[Int]) extends Player(id, kalah_index, pits) {
  override def choosePit(board: mutable.HashMap[Int, Int]): Int = {
    var chosen_pit = findLandingOnKalah(board)
    if(chosen_pit != -1) println("landing")

    if(chosen_pit == -1) {
      chosen_pit = findStealingFromOpposite(board)
      if(chosen_pit != -1) println("stealing")
    }

    if(chosen_pit == -1) {
      chosen_pit = new Random().between(pits.head, pits.last + 1)
      println("random")
    }




    println("[Player " + id + "] chose pit " + chosen_pit)
    chosen_pit
  }

  private def findLandingOnKalah(board: mutable.HashMap[Int, Int]): Int = {
    for(pit <- pits) {
      if(kalah_index - pit == board(pit)) return pit
    }
    -1
  }

  private def findStealingFromOpposite(board: mutable.HashMap[Int, Int]): Int = {
    for(pit <- pits) {
      if(board(pit) != 0) {
        val landing_pit = pit + board(pit)
        if(pits.contains(landing_pit) &&
          board(landing_pit) == 0) return pit
      }
    }
    -1
  }

}
