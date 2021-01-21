package actors

import akka.actor._
import Server._

class Bot(id: Int, server: ActorRef) extends Player(id, server)
{
  override protected def makeMove(): Int =
    {
      val move = findBestMove()
      move
    }

  def findBestMove(): Int =
    {
      val random = scala.util.Random
      random.nextInt(Server.numOfHoles)
    }
}
