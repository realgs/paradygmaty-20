package ComputerPackage

import akka.actor.Actor
import ServerPackage.Server
import GameboardPackage.Gameboard
import DecisionTreePackage.DecisionTree

class Computer extends Actor {
  def receive: Receive = {
    case Server.UserMoveRequest(board: Gameboard) =>
      val decisionTree = new DecisionTree(board)
      decisionTree.createTree()
      Thread.sleep(300)
      sender ! Server.UserMoveReceived(decisionTree.findBestMove())
  }
}
