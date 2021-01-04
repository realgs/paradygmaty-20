package main.players
import scala.io.StdIn.readInt
import main.Interface

class Human(name: String, fieldsNumber: Int, rocksNumbers: Int) extends Player(name, fieldsNumber, rocksNumbers) {

  def decideMove: Int = {
    Interface.chooseMove
    val availableMoves = getAvailableMoves
    var move = -1
    var correctMove = true

    do {
      correctMove = true

      try {
        move = readInt()
      }catch {
        case e: NumberFormatException => {
          correctMove = false
        }
      }

      if(!correctMove || !availableMoves.contains(move)) {
        Interface.printInputError
      }

    }while(!correctMove || !availableMoves.contains(move))

    move
  }
}
