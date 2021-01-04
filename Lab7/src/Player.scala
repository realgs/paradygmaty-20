class Player(val name: String, val fieldsNumber: Int, rocksNumbers: Int) {
  val fields:Array[Int] = Array.fill[Int](fieldsNumber)(rocksNumbers)
  var base = 0;

  def getAvailableMoves: Array[Int] =
    fields.zipWithIndex.filter((element)=>element._1 != 0).map((element) => element._2)

  def decideMove: Int = {
    Interface.chooseMove
    val availableMoves = getAvailableMoves
    var move = scala.io.StdIn.readInt()

    while(!availableMoves.contains(move)){
      Interface.printInputError
      move = scala.io.StdIn.readInt()
    }

    move
  }

  override def toString = s"Player $name: [${fields.mkString(" ")}], points: $base"
}