import scala.collection.mutable
import scala.io.StdIn

class HumanPlayer(id: Int, kalah_index: Int, pits: List[Int]) extends Player(id, kalah_index, pits) {
  override def choosePit(board: mutable.HashMap[Int, Int]): Int = {
    print("[Player " + id + "] Enter pit index (" + pits.head + " ... " + pits.last + ") = ")
    StdIn.readInt()
  }
}
