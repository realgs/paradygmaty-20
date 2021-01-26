import akka.actor.{ActorRef, Props}

import scala.io.StdIn

class HumanPlayer(id: Int, kalah: Int, pits: List[Int]) extends Player(id, kalah, pits) {

  override def choosePit(): Int = {
    print("[Player " + id + "] Enter pit index (" + pits.head + " ... " + pits.last + ") = ")
    StdIn.readInt()
  }
}
