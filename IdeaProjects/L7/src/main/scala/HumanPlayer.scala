import scala.io.StdIn.readLine

class HumanPlayer(name: String) extends Player(name: String) {

  override def makeDecision(): Int = {
    Integer.parseInt(readLine())
  }
}
