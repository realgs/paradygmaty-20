import akka.actor.{Actor, ActorRef}
import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, TimeoutException}


class HumanPlayer(private val numberOfPlayer:Int,private val server:ActorRef) extends Actor
{
  implicit var timeout:Timeout = akka.util.Timeout(30.seconds)
  implicit val ex:ExecutionContext = ExecutionContext.global

    def makeMove(): Int =
    {
      print(s"Graczu $numberOfPlayer podaj numer dołku: ")
      scala.io.StdIn.readInt()

    }

  override def receive: Receive =
  {
     case Start | CorrectMove => server ! AskForApproval(numberOfPlayer)

     case DoNotGiveApproval => Thread.sleep(500); server ! AskForApproval(numberOfPlayer)

     case GiveApproval | InCorrectMove => {
       val f = Future{makeMove()}
       try
       {
          val numberOfZone = Await.result(f,timeout.duration)
          server ! MakeMove(numberOfZone)
       }
       catch
         {
           case _:TimeoutException => server ! TimeIsOut
           case _:NumberFormatException => server ! WrongFormat
         }
       }


     case GameOver => println(s"Gracz $numberOfPlayer dziękuję za grę")
  }
}

// Obiekt towarzyszący
object HumanPlayer
{
  def apply(numberOfPlayer:Int,server:ActorRef):HumanPlayer =
  {
    new HumanPlayer(numberOfPlayer,server)
  }
}
