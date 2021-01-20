class GameBoard (private val seeds_amount: Int, private val houses_amount: Int) {

  private var left_base = 0
  private var right_base = 0
  private var houses = Array.fill(houses_amount)(seeds_amount)

  def printState(): Unit = {
    houses.foreach(println)
  }


}
