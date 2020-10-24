package Lista3
import scala.annotation.tailrec

object Lista3 extends App{

  // zadanie 1
  private def isNegative(number: Double): Boolean =
    number<0

  private def isNegativeAndOdd(number: Double): Boolean =
    ((number%2) != 0) && isNegative(number)

  def filterList(list: List[Double]): (List[Double], List[Double]) ={
    def innerFilterList(innerList: List[Double], filter: Double => Boolean): List[Double] =
      innerList match{
        case Nil => Nil
        case head::tail  => if(filter(head)) head::innerFilterList(tail, filter)  else innerFilterList(tail, filter)
      }
    (innerFilterList(list, isNegative), innerFilterList(list, isNegativeAndOdd))
  }

  // zadanie 2
  def length[A](list: List[A]): Int = {
    @tailrec
    def innerLength[A](innerList: List[A], accum: Int): Int =
      if (innerList == Nil) accum
      else innerLength (innerList.tail, accum+1)
    innerLength(list, 0)
  }

  //zadanie 3
  def merge[A](leftList: List[A], rightList: List[A]): List[A] =
    (leftList, rightList) match {
      case(headLeft::tailLeft, headRight::tailRight) => headLeft::headRight::merge(tailLeft, tailRight)
      case(Nil, rightList) => rightList
      case(leftList, Nil) => leftList
      case(Nil, Nil) => Nil
    }

  //zadanie 5
  def joinLists[A](firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] = {
    (firstList, secondList, thirdList) match{
      case(headFL::tailFL, _, _) => headFL::joinLists(tailFL, secondList, thirdList)
      case(Nil, headSL::tailSL, _)  => headSL::joinLists(Nil, tailSL, thirdList)
      case(Nil, Nil, headTL::tailTL) => headTL::joinLists(Nil, Nil, tailTL)
      case(Nil, Nil, Nil) => Nil
    }
  }
}
