class function1 extends App {
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
}
