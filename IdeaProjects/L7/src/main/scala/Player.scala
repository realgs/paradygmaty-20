import akka.actor.TypedActor.dispatcher
import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import scala.concurrent.Future

case class MakeAMove()
case class UpdateHoles(arr: Array[Int])

abstract class Player(name: String) extends Actor {

  var myPoints=0
  var myHoles=initializeHoles()

  override def receive: Receive={
    case MakeAMove => {
      sender() ! ShowBoard
      if(!anyAvailable()) sender ! Stop
      var decision=makeDecision()
      while((sender() ! IsAvailable(decision))==false){
        println("Empty pit!")
        decision=makeDecision()
      }
      sender() ! Move(decision)
    }
    case UpdateHoles(arr) => myHoles=arr
  }

  def anyAvailable(): Boolean={
    var i=0
    var bool=false
    while (i < myHoles.length) {
      if(myHoles(i)!=0) bool=true
      i+=1
    }
    bool
  }

  def makeDecision(): Int

  def initializeHoles(): Array[Int]={
    Array(6, 6, 6, 6, 6, 6)
  }

}
