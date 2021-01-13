package main.players

import scala.util.Random

class RandomBot(name: String, fieldsNumber: Int, rocksNumbers: Int) extends Player(name, fieldsNumber, rocksNumbers) {
  def decideMove: Int = {
    val availableMoves = getAvailableMoves
    availableMoves(Random.nextInt(availableMoves.length))
  }

  def copy: Player = {
    val copied = new RandomBot(name, fieldsNumber, rocksNumbers)
    copied.base = base
    copied.fields = fields.clone()

    copied
  }
}
