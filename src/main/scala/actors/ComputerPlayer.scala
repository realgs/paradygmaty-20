package actors

import akka.actor.{Actor, ActorRef, Props, Timers}


class ComputerPlayer(server: ActorRef) extends Actor with Timers {

  server ! Server.ConnectToServer

  override def receive: PartialFunction[Any, Unit] = {
    case ComputerPlayer.MakeMove() => {

      /*
      println("zaczynamy")
      context.setReceiveTimeout(1000.millis)
      timers.startSingleTimer("aa", ComputerPlayer.TimesUp(), 1000.millis)
      println("Po pętli")
       */
    }


    case ComputerPlayer.TimesUp() =>
      println("Koniec czasu")
  }
}

object ComputerPlayer {
  def props(server: ActorRef): Props = Props(classOf[ComputerPlayer], server)

  case class MakeMove()
  case class TimesUp()
}
