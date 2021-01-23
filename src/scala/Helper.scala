package scala

import scala.io.StdIn

object Helper {
  def getInputFromUser(): Int = {
    var choice = 0
    var continue = true
    while (continue) {
      try {
        choice = StdIn.readInt()
        continue = false
      } catch {
        case e: Exception => println("Your input must be a numeric value")
      }
    }
    choice
  }
}
