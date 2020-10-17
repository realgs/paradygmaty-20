import scala.annotation.tailrec

object Functions {

  // Function 1.
  def multiply(numbers:List[Double]):Double =
    if(numbers == Nil) 0
    else if(numbers.tail == Nil) numbers.head
    else numbers.head * multiply(numbers.tail)

  // Function 2.
  def stringConstants(stringList:List[String], endSign:Char, separator:String):String =
    if(stringList == Nil) ""
    else if(stringList.tail == Nil) stringList.head + endSign
    else stringList.head + separator + stringConstants(stringList.tail, endSign, separator)

  // Function 3.
  @tailrec
  def ifInRange(numbers:List[Double], X:Double, Y:Double):Boolean =
    if(numbers == Nil) true
    else numbers.head >= X && numbers.head <= Y && ifInRange(numbers.tail, X, Y)

  // Function 4.
  def power(X:Double, Y:Int):Double =
    if(Y == 0) 1
    else if(Y > 0) X * power(X, Y - 1)
    else power(1 / X, -Y)

}
