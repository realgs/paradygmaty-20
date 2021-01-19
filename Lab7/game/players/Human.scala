package main.game.players
import scala.io.StdIn.readInt
import main.Interface

class Human(name: String, fieldsNumber: Int, rocksNumbers: Int) extends Player(name, fieldsNumber, rocksNumbers) {

  def decideMove: Int = {
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

      if(!correctMove ) {
        Interface.printInputError()
      }

    }while(!correctMove )

    move
  }

  def copy: Player = {
    val copied = new Human(name, fieldsNumber, rocksNumbers)
    copied.base = base
    copied.fields = fields.clone()

    copied
  }
}
