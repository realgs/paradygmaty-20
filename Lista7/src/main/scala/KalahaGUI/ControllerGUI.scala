package KalahaGUI

import javax.swing.{JButton, JPanel, JTextArea,JTextField}
import ServerPackage.Server
import GameboardPackage.Gameboard
import UserPackage.User
import akka.actor.{Actor,ActorRef,ActorSystem,Props}

object ControllerGUI {
  def pcVsPc(panel: JPanel,okno: JTextArea): Unit = {
    //val future = Await.ready()
  }
  def userVsPc(panel: JPanel,
               userButton: JButton,
               disabledButton:JButton,
               GameMessageOutput:JTextArea): Unit = {

    disabledButton.setVisible(false)
    GameMessageOutput.setText("User vs PC")
  }
  def userVsUser(panel: JPanel,
                 userButton1:JButton,
                 userButton2:JButton,
                 user1TextInput:JTextField,
                 user2TextInput:JTextField,
                 gameMessageOutput:JTextArea): Unit = {

    userButton1.setVisible(true)
    userButton2.setVisible(true)
    val system = ActorSystem("Kalaha")
    val gameBoard = new Gameboard
    val player1 = system.actorOf(Props(classOf[User],userButton1,user1TextInput), "Player1")
    val player2 = system.actorOf(Props(classOf[User],userButton2,user2TextInput), "Player2")
    val server = system.actorOf(Props(classOf[Server], player1, player2, gameBoard, gameMessageOutput), "Server")
    server ! Server.ServerAction()
  }
}
