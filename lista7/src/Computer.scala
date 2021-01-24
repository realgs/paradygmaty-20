import scala.util.Random

class Computer() extends Engine {
  override def move(player: Int): Int = {
    Random.nextInt(6) + 1
  }
}
