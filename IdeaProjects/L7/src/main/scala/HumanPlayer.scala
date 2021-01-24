import scala.io.StdIn.readLine

class HumanPlayer extends Player {
  override def receive: Receive = ???

  override def makeDecision(): Int = {
    Integer.parseInt(readLine())
  }
}
