package List2

object L2 {
  val product: List[Double] => Double = (list: List[Double]) =>
    if (list == Nil) 0
    else if (list.size == 1) list.head
    else list.head * product(list.tail)

  val createString: (List[String], Char, Char) => String = (list: List[String], separator: Char, endSymbol: Char) =>
    if (list == Nil) throw new Exception("Empty list")
    else if (list.size == 1) list.head + endSymbol
    else list.head + separator + createString(list.tail, separator, endSymbol)

  val checkRange: (List[Double], Double, Double) => Boolean = (list: List[Double], x: Double, y: Double) =>
    if (list == Nil) true
    else if (list.head >= x && list.head <= y) checkRange(list.tail, x, y)
    else false

  val countPower: (Double, Int) => Double = (base: Double, exponent: Int) =>
    if (exponent < 0) countPower(1/base, Math.abs(exponent))
    else if (exponent > 0) base * countPower(base,exponent-1)
    else 1
}
