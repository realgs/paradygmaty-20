package lab7

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import lab7.Reader.{AskInt, Read, ResponseInt, Withdraw}

import scala.collection.mutable
import scala.io.StdIn.readInt

class Reader(system: ActorSystem) extends Actor {
  var intReader: ActorRef = system.actorOf(ReadInt.props())
  intReader ! ReadInt.Start

  val queue: mutable.Queue[(ActorRef, ActorRef, String)] = new mutable.Queue()

  def receive: Receive = {
    case AskInt(msg, act) => {
      if (queue.isEmpty) println(msg)
      queue.enqueue((sender(), act, msg))
    }
    case Withdraw(msg) => {
      queue.dequeueFirst(pair => pair._2 == sender())
      if(msg.length > 0) println(msg)
    }
    case Read(answer) => {
      if (queue.nonEmpty) {
        val (waitingActor, _, _) = queue.dequeue()
        waitingActor ! ResponseInt(answer)
        if(queue.nonEmpty) {
          val (_,_, msg) = queue.front
          println(msg)
        }
      }
    }
  }

  class ReadInt() extends Actor {
    def receive: Receive = {
      case ReadInt.Start => {
        println("ReadInt started")
        while (true) {
          try {
            sender() ! Read(readInt())
          } catch {
            case _: NumberFormatException => ()
          }
        }
      }
    }
  }

  object ReadInt {
    def props(): Props = Props(new ReadInt())
    final case object Start
  }
}

object Reader {
  def props(system: ActorSystem): Props = Props(new Reader(system))

  final case class AskInt(msg: String, act: ActorRef)
  final case class ResponseInt(answer: Int)
  final case class Withdraw(msg: String = "")
  final case class Read(answer: Int)
}
