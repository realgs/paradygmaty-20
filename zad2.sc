import scala.annotation.tailrec

def listLength[A](list: List[A]): Int = {
  @tailrec
  def listLengthHelper(list:List[A], accum:Int):Int =
    list match {
      case Nil => accum
      case h :: t => listLengthHelper(t, accum+1)
    }
  listLengthHelper(list,0)
}

//Złożoność obliczeniowa O(n), pamięciowa O(1)
listLength(List(1, 2, 3, 4, 5)) == 5
listLength(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')) == 8
listLength(List(1, 2)) == 2
listLength(List(1)) == 1
listLength(Nil) == 0
