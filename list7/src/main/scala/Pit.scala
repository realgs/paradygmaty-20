class Pit(val number: Int, var seedsNumber: Int) {

  def isEmpty: Boolean = seedsNumber == 0

  override def toString: String = "(" + seedsNumber + ")"
}
