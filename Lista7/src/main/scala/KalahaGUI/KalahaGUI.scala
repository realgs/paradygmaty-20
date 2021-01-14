package KalahaGUI

import java.awt.event.ActionEvent
import java.awt.{BorderLayout, GridLayout}
import javax.swing.text.SimpleAttributeSet
import javax.swing.JFrame
import javax.swing.JTextPane
import javax.swing._
import javax.swing.text.StyleConstants

import scala.swing.BorderPanel

class KalahaGUI {
  private val frame = new JFrame
  private val button1 = new JButton("PC vs PC")
  private val button2 = new JButton("USER vs PC")
  private val button3 = new JButton("USER vs USER")
  private val button4 = new JButton("Player 1")
  private val button5 = new JButton("Player 2")
  private val panelWEST = new JPanel()
  private val panelSOUTH = new JPanel()
  private val panelCENTER = new JPanel()
  private val user1TextInput = new JTextField()
  private val user2TextInput = new JTextField()
  private val gameWindow = new JTextPane()
  panelWEST.setLayout(new GridLayout(3,1))
  panelWEST.add(button1)
  panelWEST.add(button2)
  panelWEST.add(button3)
  panelSOUTH.setLayout(new GridLayout(2,1))

  panelSOUTH.add(user1TextInput)
  user1TextInput.setHorizontalAlignment(javax.swing.SwingConstants.CENTER)
  panelSOUTH.add(user2TextInput)
  user2TextInput.setHorizontalAlignment(javax.swing.SwingConstants.CENTER)
  panelSOUTH.add(button4)
  panelSOUTH.add(button5)
  button4.setVisible(false)
  button5.setVisible(false)

  panelCENTER.setLayout(new GridLayout(1,1))
  val attribs = new SimpleAttributeSet
  StyleConstants.setAlignment(attribs,StyleConstants.ALIGN_CENTER)
  panelCENTER.add(gameWindow)
  gameWindow.setEditable(false)
  gameWindow.setParagraphAttributes(attribs,true)

  button1.addActionListener((e: ActionEvent) => ControllerGUI.pcVsPc(panelSOUTH,gameWindow))
  button2.addActionListener((e: ActionEvent) => ControllerGUI.userVsPc(panelSOUTH,button4,button5,gameWindow))
  button3.addActionListener((e: ActionEvent) => ControllerGUI.userVsUser(panelSOUTH,button4,button5,user1TextInput,user2TextInput,gameWindow))

  frame.add(panelWEST,BorderLayout.WEST)
  frame.add(panelSOUTH,BorderLayout.SOUTH)
  frame.add(panelCENTER,BorderLayout.CENTER)
  frame.setSize(500,180)
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  frame.setVisible(true)
}
