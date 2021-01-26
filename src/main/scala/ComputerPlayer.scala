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
      chosen_pit = new Random().between(pits.head, pits.last + 1)
      println("random")
    }



    println("[Player " + id + "] chose pit " + chosen_pit)
    chosen_pit
  }

  private def findLandingOnKalah(): Int = {
    for(pit <- pits) {
      if(kalah - pit == m_board(pit)) return pit
    }
    -1
  }

  private def findStealingFromOpposite(): Int = {
    for(pit <- pits) {
      if(m_board(pit) != 0) {
        val landing_pit = pit + m_board(pit)
        if(pits.contains(landing_pit) &&
          m_board(landing_pit) == 0) return pit
      }
    }
    -1
  }

}
