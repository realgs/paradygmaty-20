import scala.io.StdIn.readInt

class Player() extends Engine {

  override def move(player: Int): Int = {
    println("Player" + player + " choose pit: ")
    readInt()
    }
}
