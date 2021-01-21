package lab7

import akka.actor.{Actor, ActorRef, Props}
import lab7.Kalaha.system
import lab7.Kalaha.system.dispatcher
import lab7.Client._
import lab7.Server._

import scala.concurrent.duration.{DurationInt}

class Server(val firstPlayer: ActorRef, val secondPlayer: ActorRef) extends Actor {
  private var position = Position()

  private var counter = 0

  def opponent(act: ActorRef) = if (firstPlayer == act) secondPlayer else firstPlayer
  def currentPlayer = if (position.turn == Yellow) firstPlayer else secondPlayer

  firstPlayer ! Init(self, Yellow)
  secondPlayer ! Init(self, Blue)

  firstPlayer ! MoveRequest(position, moveTimeSeconds)
  system.scheduler.scheduleOnce(moveTimeSeconds.seconds, self, TimeUp(counter))

  def receive = {
    case Move(move) => {
      counter += 1
      try {
        position = position.next(move)
        if (position.finished) {
          position.winner match {
            case None => {
              firstPlayer ! End(Draw)
              secondPlayer ! End(Draw)
            }
            case Some(Yellow) => {
              firstPlayer ! End(Win)
              secondPlayer ! End(Loose)
            }
            case Some(Blue) => {
              firstPlayer ! End(Loose)
              secondPlayer ! End(Win)
            }
          }
        } else {
          currentPlayer ! MoveRequest(position, moveTimeSeconds)
          system.scheduler.scheduleOnce(moveTimeSeconds.seconds, self, TimeUp(counter))
        }
      } catch {
        case e: Exception => {
          println(e.getMessage)
          currentPlayer ! InvalidMove(wrongMoveTimeSeconds)
          system.scheduler.scheduleOnce(wrongMoveTimeSeconds.seconds, self, TimeUp(counter))
        }
      }
    }
    case TimeUp(cnt) => {
      if (cnt == counter) {
        currentPlayer ! Timeout()
        self ! Move(position.randomMove)
        counter += 1
      }
    }
  }
}

object Server {
  val moveTimeSeconds = 10
  val wrongMoveTimeSeconds = 20

  def props(firstPlayer: ActorRef, secondPlayer: ActorRef): Props = Props(new Server(firstPlayer, secondPlayer))

  final case class Move(move: Int)
  final case class TimeUp(counter: Int)
}