object Lista2 extends App {
  def mult(list: List[Int] ): Int = {
    if (list == Nil) 0
    else if(list.length > 1) list.head * mult(list.tail)
    else list.head
  }

  def connectString(listOString: List[String], sep: String, endChar: String): String = {
    if (listOString == Nil) " "
    else if (listOString.tail != Nil) listOString.head + sep + connectString(listOString.tail, sep, endChar)
    else listOString.head + endChar
  }

  def interval(listOfNumbers : List[Double], x: Double, y: Double) : Boolean = {
    if (listOfNumbers == Nil) true
    else if (listOfNumbers.head >= x && listOfNumbers.head <= y) interval(listOfNumbers.tail, x, y)
    else false;
  }

  def pow(x: Int, y: Int): Int= {
    if (y < 0) throw new Exception("Illegal value of argument")
    if (y == 0) 1
    else x * pow(x, y-1)
  }
}
