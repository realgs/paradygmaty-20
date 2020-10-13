object zad03 {
  @scala.annotation.tailrec
  def inRange(list: List[Double], left: Double, right: Double): Boolean = {
    if (left > right) throw new Exception("Left cannot be greater than right")
    else if (list == Nil) true
    else if (list.head < left || list.head > right) false
    else inRange(list.tail, left, right)
  }

  def runTests(): Unit = {
    assert(inRange(List(), 0, 10))
    assert(inRange(List(5, 1, -3, 2, 5, 0, 10, 4), -5, 10))
    assert(inRange(List(5, 1), 1, 5))
    Utils.assertThatExceptionIsThrown(() => inRange(List(1, 2), 10, 1))
  }
}
