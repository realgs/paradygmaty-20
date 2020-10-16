
object Tests {

  def multiplyingTest(): Unit = {
    println("Multiplying test")
    println("1." + (L2.multiply(List(-1, 2, 5.1)) == -10.2).toString)
    println("2." + (L2.multiply(List(2, 0, 8, 12)) == 0).toString)
    println("3." + (L2.multiply(List()) == 0.0).toString)
    println("4." + (L2.multiply(List(0.1, 0.1, 0.5)) == 0.005).toString)
  }

  def formattingTextTest(): Unit = {
    println("\nFormatting text test")
    println("1." + (L2.formatText(List("What's", "up"), " ", "?") == "What's up?").toString)
    println("2." + (L2.formatText(List("one", "two", "three"), ",", "...") == "one,two,three...").toString)
    println("3." + (L2.formatText(List(), " ", ".") == "").toString)
    println("4." + (L2.formatText(List("Hi"), ",", "!") == "Hi!").toString)
    println("5." + (L2.formatText(List("", ""), "*", ".") == "*.").toString)

  }

  def powTest(): Unit = {
    println("\nExponentiation test")
    println("1." + (L2.pow(0.5, -2) == 4).toString)
    println("2." + (L2.pow(1, 10) == 1).toString)
    println("3." + (L2.pow(-3, 3) == (-27)).toString)
    println("4." + (L2.pow((-2), 2) == 4).toString)
    println("5." + (L2.pow((-0.3), 2) == 0.09).toString)
  }

  def rangingNumberTest(): Unit = {
    println("\nCheck if list is within the range of [X;Y]")
    println("1." + (L2.rangeNumbers(List(2, 3, 6, 0.99), 1, 7) == false).toString)
    println("2." + (L2.rangeNumbers(List(-1, 3, 6, 7), 1, 7) == false).toString)
    println("3." + (L2.rangeNumbers(List(0, 1, 2, 3, 4, 5), 0, 5) == true).toString)
    println("4." + (L2.rangeNumbers(List(0.2, 7.1), 0.21, 7.11) == false).toString)
  }


}
