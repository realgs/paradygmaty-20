package main.players

abstract class Player(val name: String, val fieldsNumber: Int, rocksNumbers: Int) {
  val fields:Array[Int] = Array.fill[Int](fieldsNumber)(rocksNumbers)
  var base = 0;

  def getAvailableMoves: Array[Int] =
    fields.zipWithIndex.filter(element => element._1 != 0).map(element => element._2)

  def decideMove: Int

  override def toString = s"main.players.Player $name: [${fields.mkString(" ")}], points: $base"
}