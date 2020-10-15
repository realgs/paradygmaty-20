package functions

class Functions {
  // zadanie 1
  def multiply (list: List[Double]) : Double = {
    if (list.isEmpty) 0.0
    else if (list.length == 1) list.head
    else list.head * multiply(list.tail)
  }

  // zadanie 2
  def createSentence (list: List[String], finishSign: Char, separator: Char) : String = {
    if (list.isEmpty) ""
    else if (list.length == 1) list.head + finishSign
    else list.head + separator + createSentence(list.tail, finishSign, separator)
  }
}
