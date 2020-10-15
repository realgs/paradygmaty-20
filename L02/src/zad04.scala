object zad04 {
  def toPower(base: BigDecimal, exponent: Int): BigDecimal = {
    validate(base, exponent)
    if (exponent < 0) 1 / calcPower(base, -exponent)
    else calcPower(base, exponent)
  }

  private def validate(base: BigDecimal, exponent: Int): Unit = {
    if (base == 0 && exponent <= 0) throw new Exception("Invalid input")
  }

  private def calcPower(base: BigDecimal, nonNegativeExponent: Int): BigDecimal = {
    if (nonNegativeExponent == 0) 1
    else base * calcPower(base, nonNegativeExponent - 1)
  }

  def runTests(): Unit = {
    assert(toPower(2, 10) == 1024)
    assert(toPower(1, -1) == 1)
    assert(toPower(-2, -3) == -0.125)
    assert(toPower(-0.2, 3) == -0.008)
    Utils.assertThatExceptionIsThrown(() => toPower(0, 0))
    Utils.assertThatExceptionIsThrown(() => toPower(0, -5))
  }
}
