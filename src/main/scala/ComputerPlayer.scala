import akka.actor.{ActorRef, Props}

import scala.util.Random

class ComputerPlayer(id: Int, kalah: Int, pits: List[Int]) extends Player(id, kalah, pits) {

  override def choosePit(): Int = {
    var chosen_pit = findLandingOnKalah()
    if(chosen_pit != -1) println("landing")

    if(chosen_pit == -1) {
      chosen_pit = findStealingFromOpposite()
      if(chosen_pit != -1) println("stealing")
    }

    if(chosen_pit == -1) {
      chosen_pit = findLargestPit()
      println("largest pit")
    }

    println("[Player " + id + "] chose pit " + chosen_pit + "\n")
    chosen_pit
  }

  private def findLandingOnKalah(): Int = {
    for(pit <- pits) {
      if(kalah - pit == board(pit)) return pit
    }
    -1
  }

  private def findStealingFromOpposite(): Int = {
    for(pit <- pits) {
      if(board(pit) != 0) {
        val landing_pit = pit + board(pit)
        if(pits.contains(landing_pit) &&
          board(landing_pit) == 0 &&
          board(14 - landing_pit) != 0) return pit
      }
    }
    -1
  }

  private def findLargestPit(): Int = {
    var largest = 0
    var largest_stones = 0

    for(pit <- pits) {
      if(board(pit) > largest_stones) {
        largest_stones = board(pit)
        largest = pit
      }
    }

    largest
  }

}
