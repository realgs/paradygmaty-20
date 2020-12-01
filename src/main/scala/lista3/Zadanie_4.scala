package lista3

import scala.annotation.tailrec

object Zadanie_4 {
  def find(list: List[String], fraze: String): List[String] = {
    (list, fraze) match {
      case (Nil, _) => Nil
      case (_, "") => Nil
      case _ => if (ifFrazeContains(fraze, list.head)) list.head :: find(list.tail, fraze) else find(list.tail, fraze)
    }
  }

  def taiRecFind(list: List[String], fraze: String): List[String] = {
    @tailrec
    def iterTailRecFind(listT: List[String], frazeT: String, accum: List[String]): List[String] = {
      (listT, frazeT) match {
        case (Nil, _) => accum
        case (_, "") => accum
        case _ => if (ifFrazeContains(frazeT, listT.head)) iterTailRecFind(listT.tail, frazeT, accum :+ listT.head) else iterTailRecFind(listT.tail, frazeT, accum)
      }
    }

    iterTailRecFind(list, fraze, Nil)
  }

  def ifFrazeContains(fraze: String, sentence: String): Boolean = {
    if (sentence == "") false
    else if (ifCharContains(fraze.toList, sentence.toList)) true
    else ifFrazeContains(fraze, sentence.tail)
  }

  def ifCharContains(xs: List[Char], ys: List[Char]): Boolean = {
    (xs, ys) match {
      case (Nil, _) => true
      case (_, Nil) => false
      case _ => if (xs.head == ys.head) ifCharContains(xs.tail, ys.tail) else false
    }
  }
}
