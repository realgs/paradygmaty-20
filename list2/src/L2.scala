object L2 {

  def multiply(realNumbersList: List[BigDecimal]): BigDecimal = {
    if (realNumbersList.isEmpty) 0.0
    else if (realNumbersList != Nil & realNumbersList.length != 1) realNumbersList.head * multiply(realNumbersList.tail)
    else realNumbersList.head
  }

  def formatText(text: List[String], separator: String, mark: String): String = {
    if (text == Nil) ""
    else if (text.length == 1) text.head + mark
    else text.head + separator + formatText(text.tail, separator, mark)
  }

  def rangeNumbers(numbersList: List[Double], X: Double, Y: Double): Boolean = {
    if (numbersList.isEmpty) throw new Exception("Empty list exception")
    if (X > Y) throw new Exception("Wrong range given")
    for (i <- numbersList) {
      if (i < X || i > Y) return false
    }
    true
  }

  def pow(X: BigDecimal, Y: Int): BigDecimal = {
    if (X == 0 && Y <= 0) throw new Exception("Undefined value")
    if (X == 0 && Y > 0) 0
    else if (Y == 0) 1
    else if (Y < 0) 1 / X * pow((1 / X), (-Y) - 1)
    else X * pow(X, Y - 1)
  }


}
