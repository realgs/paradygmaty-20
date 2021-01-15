package ComputerPackage

import akka.actor.Actor
import ServerPackage.Server
import GameboardPackage.Gameboard

class Computer extends Actor {
  def receive: Receive = {
    case Server.UserMoveRequest(board: Gameboard) => {

    }
  }
}
