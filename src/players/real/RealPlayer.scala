package pl.pawelklecha.kalaha
package players.real

import events.{PlayerActionEvent, ServerActionEvent}

import akka.actor.Actor

import scala.io.StdIn

class RealPlayer extends Actor {

  override def receive: Receive = {
    case ServerActionEvent.MakeMove(gameboard) =>
      var chosenHole = -1

      while (!gameboard.checkMoveCorrectness(chosenHole)) {
        print("Give hole number: ")
        try {
          chosenHole = StdIn.readInt()
        } catch {
          case _: NumberFormatException => chosenHole = -1
        }
      }

      sender ! PlayerActionEvent.ConfirmMove(chosenHole)
  }
}
