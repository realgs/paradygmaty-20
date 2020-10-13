object zad01 {
  def product(list: List[Double]): Double = {
    if (list == Nil) 1
    else list.head * product(list.tail)
  }

  def runTests(): Unit = {
    assert(product(List(1, 2, 3)) == 6)
    assert(product(List()) == 1)
    assert(product(List(5)) == 5)
    assert(product(List(-1, 2.5, 4, -10, 0.5)) == 50)
  }
}
