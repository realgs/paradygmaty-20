package KalahaGUI

import javax.swing.{JButton, JPanel, JTextArea,JTextField}

object ControllerGUI {
  def pcVsPc(panel: JPanel,okno: JTextArea): Unit = {
    //val future = Await.ready()
    println("pc vs pc")
  }
  def userVsPc(panel: JPanel, userButton: JButton,disabledButton:JButton,GameMessageOutput:JTextArea): Unit = {
    disabledButton.setVisible(false)
    GameMessageOutput.setText("User vs PC")
  }
  def userVsUser(panel: JPanel, userButton1:JButton,userButton2:JButton,gameMessageOutput:JTextArea): Unit = {
    userButton2.setVisible(true)
    gameMessageOutput.setText("user vs hahahah")
    println(gameMessageOutput.getText)
    println("user vs user")
  }
}
