package MainPackage

import KalahaGUI.KalahaGUI
import GameboardPackage.Gameboard

object Main extends App {
  val obj = new KalahaGUI
  val obj2 = new Gameboard()
  println(obj2.createBoard(6).toList)
}
