object Zadanie3 extends App{
  @scala.annotation.tailrec
  def intervalChecker(list: List[Double], X: Double, Y: Double): Boolean =
    if(list==Nil) true
    else if((list.head>=X) && (list.head<=Y)) intervalChecker(list.tail, X, Y)
    else false

  println(intervalChecker(List(1.4, 2, 3.5, 2.2), 0, 4)==true)
  println(intervalChecker(List(1.6, -2, 4.5, 2.2), 0, 5)==false)
  println(intervalChecker(List(1.6, 13, 4.5, 4.2), 0, 10)==false)
  println(intervalChecker(List(), 0, 3.3)==true)
  println(intervalChecker(List(1.1,-2.2), -2.2, 1.1)==true)
}
