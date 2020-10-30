package lista3

import scala.annotation.tailrec

object Zadanie_4 {
  def find(list: List[String], fraze: String): List[String] = {
    (list, fraze) match {
      case (Nil, _) => Nil
      case (_, "") => Nil
      case _ => if (myContains(fraze, list.head)) list.head :: find(list.tail, fraze) else find(list.tail, fraze)
    }
  }

  def taiRecFind(list: List[String], fraze: String): List[String] = {
    @tailrec
    def iterTailRecFind(listT: List[String], frazeT: String, accum: List[String]): List[String] = {
      (listT, frazeT) match {
        case (Nil, _) => accum
        case (_, "") => accum
        case _ => if (myContains(frazeT, listT.head)) iterTailRecFind(listT.tail, frazeT, accum :+ listT.head) else iterTailRecFind(listT.tail, frazeT, accum)
      }
    }

    iterTailRecFind(list, fraze, Nil)
  }

  def myContains(fraze: String, sentance: String): Boolean = {
    if (sentance == "") false
    else if (initSegment(fraze.toList, sentance.toList)) true
    else myContains(fraze, sentance.tail)
  }

  def initSegment(xs: List[Char], ys: List[Char]): Boolean = {
    (xs, ys) match {
      case (Nil, _) => true
      case (_, Nil) => false
      case _ => if (xs.head == ys.head) initSegment(xs.tail, ys.tail) else false
    }
  }
}
