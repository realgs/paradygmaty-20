import akka.actor.{Actor, ActorRef, ActorSystem, Props}

case class MakeAMove()

abstract class Player(name: String) extends Actor {

  var myPoints=0
  var myHoles=initializeHoles()

  override def receive: Receive={
    case MakeAMove => {
      println(name + " MakeAMove")
      sender() ! ShowBoard
      var decision=makeDecision()
      while((sender() ! IsAvailable(decision))==false){
        decision=makeDecision()
      }
      sender() ! Move(decision)
    }
  }

  def makeDecision(): Int

  def initializeHoles(): Array[Int]={
    Array(6, 6, 6, 6, 6, 6)
  }

}
