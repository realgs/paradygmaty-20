import scala.collection.mutable

abstract class Player(val id: Int, val kalah_index: Int, val pits: List[Int]) {
  def choosePit(board: mutable.HashMap[Int, Int]): Int
}
