package Lista4

import scala.util.Try

object Lista4LazyLists extends App {
  // Exceptions
  class WrongValueOfArguments extends Exception


  // zadanie 4 (5 pkt)
  def eachNElement[A](stream: LazyList[A], n: Int, m: Int): LazyList[A] = {
    if(n <= 0 || m  < 0 )
      throw new WrongValueOfArguments
    else{
      def innerEachNElement(innerStream: LazyList[A], counter: Int, listLengthCounter: Int): LazyList[A] =
        innerStream match {
          case head #:: tail =>
            (listLengthCounter, counter) match {
              case (0, _) => LazyList()
              case (_, 0) => head #:: innerEachNElement(tail, n-1, listLengthCounter-1)
              case (_, _) => innerEachNElement(tail, counter-1, listLengthCounter-1)
          }
          case _ => LazyList()
        }
      innerEachNElement(stream, 0, m)
    }
  }


  // zadanie 5 (5 pkt)
  def addition(a: Double, b: Double): Double =
    a+b

  def subtraction(a: Double, b: Double): Double =
    a-b

  def multiplication(a: Double, b: Double): Double =
    a*b

  def division(a: Double, b: Double): Double = {
    if (b == 0.0)
      throw new ArithmeticException()
    else
      a/b
  }

  //jezeli jedna z list leniwych jest krotsza, to na koniec doklejam reszte dluzszej listy
  def lDzialanie(leftStream: LazyList[Double], rightStream: LazyList[Double], function: (Double, Double) => Double): LazyList[Double] = {
    (leftStream, rightStream) match {
      case (leftHead #:: leftTail, rightHead #:: rightTail) => function(leftHead, rightHead) #:: lDzialanie(leftTail, rightTail, function)
      case (LazyList(), LazyList()) => LazyList()
      case (LazyList(), _) => rightStream
      case (_, LazyList()) => leftStream
    }
  }

}
