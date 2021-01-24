import akka.actor.{Actor, ActorRef, ActorSystem, Props}

case class MakeAMove()

abstract class Player extends Actor {

  var myPoints=0
  var myHoles=initializeHoles()

  override def receive: Receive={
    case MakeAMove => {
      val decision=makeDecision()
      sender() ! Move(decision)
    }
  }

  def makeDecision(): Int

  def initializeHoles(): Array[Int]={
    Array(6, 6, 6, 6, 6, 6)
  }
}
