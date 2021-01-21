package actors

import akka.actor._

class Human(val username: String, id: Int, server: ActorRef) extends Player(id, server)
{
  override protected def makeMove(): Int =
    {
      println(s"$username choose a hole 1-6")
      scala.io.StdIn.readInt() - 1
    }
}
