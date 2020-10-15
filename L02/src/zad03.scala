object zad03 {
  @scala.annotation.tailrec
  def inRange(list: List[Double], lower: Double, upper: Double): Boolean = {
    if (lower > upper) throw new Exception("Lower cannot be greater than upper")
    else if (list == Nil) true
    else if (list.head < lower || list.head > upper) false
    else inRange(list.tail, lower, upper)
  }

  def runTests(): Unit = {
    assert(inRange(List(), 0, 10))
    assert(inRange(List(5, 1, -3, 2, 5, 0, 10, 4), -5, 10))
    assert(inRange(List(5, 1), 1, 5))
    Utils.assertThatExceptionIsThrown(() => inRange(List(1, 2), 10, 1))
  }
}
