package functions

class Functions {
  // zadanie 1
  def multiply (list: List[Double]) : Double = {
    if (list.isEmpty) 0.0
    else if (list.length == 1) list.head
    else list.head * multiply(list.tail)
  }
}
