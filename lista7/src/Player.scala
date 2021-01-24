import scala.io.StdIn.readInt

class Player() extends Engine {

  override def move(player: Int): Int = {
    print("Player" + (player + 1) + " choose pit: ")
    readInt()
    }
}
