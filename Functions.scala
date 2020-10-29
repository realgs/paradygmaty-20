class Functions extends App {
  //Helpers
  def rev[A](list: List[A]): List[A] = {
    def revIter[A](listToRev: List[A], list: List[A]): List[A] = {
      listToRev match {
        case Nil => list
        case _ => revIter(listToRev.tail, listToRev.head :: list)
      }
    }

    revIter(list, List())
  }

  //zadanie 1
  def podziel(listToMerge: List[Int]): (List[Int], List[Int]) = {
    def split(source: List[Int], first: List[Int], second: List[Int]): (List[Int], List[Int]) = {
      if (source == Nil) (first, second)
      else if (source.head < 0)
        if (source.head % 2 == 1)
          split(source.tail, source.head :: first, source.head :: second)
        else split(source.tail, source.head :: first, second)
      else (first, second)
    }

    split(listToMerge, List(), List())
  }

  //zadanie 2
  def dlugosc[A](list: List[A]): Int = {
    @scala.annotation.tailrec
    def lengthIter[A](list: List[A], length: Int): Int = {
      list match {
        case Nil => length
        case _ => lengthIter(list.tail, length + 1)
      }
    }

    lengthIter(list, 0)
  }

  //zadanie 3
  def polacz[A](first: List[A], second: List[A]): List[A] = {
    @scala.annotation.tailrec
    def appendIter[A](first: List[A], second: List[A], appended: List[A]): List[A] = {
      (first, second) match {
        case (Nil, Nil) => rev(appended)
        case (Nil, _) => rev(appended) ::: second
        case (_, Nil) => rev(appended) ::: first
        case (_, _) =>
          appendIter(first.tail, second.tail, first.head :: second.head :: appended)
      }
    }

    appendIter(first, second, List())
  }
}
