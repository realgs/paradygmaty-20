package UserPackage

import javax.swing._
import java.awt.event.ActionEvent
import akka.actor.{Actor, ActorRef}
import ServerPackage.Server
import GameboardPackage.Gameboard

class User(private[this] val button: JButton, private[this] val textArea: JTextField) extends Actor {
  private[this] var senderval: ActorRef = _

  private[this] var text: String = ""
  button.addActionListener((e: ActionEvent) =>
    senderval! Server.UserMoveReceived(Integer.parseInt(textArea.getText()))
  )

  override def receive: Receive = {
    case Server.UserMoveRequest(board: Gameboard) => textArea.setText("It's your move")
      senderval = sender()
  }
}
