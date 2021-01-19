package main.game.players

abstract class Player(val name: String, val fieldsNumber: Int, rocksNumbers: Int) {
  var fields:Array[Int] = Array.fill[Int](fieldsNumber)(rocksNumbers)
  var base = 0;

  def getAvailableMoves: Array[Int] =
    fields.zipWithIndex.filter(element => element._1 != 0).map(element => element._2)

  def copy: Player

  def decideMove: Int

  override def toString = s"Player $name: [${fields.mkString(" ")}][$base]"
}