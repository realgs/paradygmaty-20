package Lista3
import scala.annotation.tailrec

object Lista3 extends App{

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
      case(Nil, headRight::tailRight) => headRight::tailRight
      case(headLeft::tailLeft, Nil) =>headLeft::tailLeft
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
