object zad04 {
  private def checkValidity(base: Double, index: Int): Unit = {
    if (base == 0 && index <= 0) throw new Exception("Invalid input")
  }

  private def calcPower(base: Double, nonNegativeIndex: Int): Double = {
    if (nonNegativeIndex == 0) 1
    else base * calcPower(base, nonNegativeIndex - 1)
  }

  def toPower(base: Double, index: Int): Double = {
    checkValidity(base, index)
    if (index < 0) 1 / calcPower(base, -1 * index)
    else calcPower(base, index)
  }

  def runTests(): Unit = {
    assert(toPower(2, 10) == 1024)
    assert(toPower(1, -1) == 1)
    assert(toPower(-2, -3) == -0.125)
    Utils.assertThatExceptionIsThrown(() => toPower(0, 0))
    Utils.assertThatExceptionIsThrown(() => toPower(0, -5))
  }
}
