package actors

import akka.actor._
import Server._

class Bot(id: Int) extends Player(id)
{
  override protected def makeMove(): Int =
    {
      val move = findBestMove()
      println(s"Bot choice is: ${move + 1}")
      move
    }

  def findBestMove(): Int =
    {
      val random = scala.util.Random
      random.nextInt(Server.numOfHoles)
    }
}

object Bot
{
  def props(id: Int): Props = Props(classOf[Bot], id)
}
