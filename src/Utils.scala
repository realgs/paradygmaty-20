import scala.annotation.tailrec

object Utils {
  def filter[A] (list: List[A], predicate: A => Boolean): List[A] =
    list match {
      case Nil => Nil
      case head :: tail => if(predicate(head)) head :: filter(tail, predicate) else filter(tail, predicate)
    }

  def rev[A] (list: List[A]): List[A] = {
    @tailrec
    def revHelper(init: List[A], result: List[A]): List[A] = {
      if (init == Nil) result
      else revHelper(init.tail, init.head :: result)
    }
    revHelper(list, List())
  }

  @tailrec
  def doesFit (element: String, phrases: List[String]): Boolean =
    if(phrases == Nil) false
    else if(contains(element, phrases.head)) true
    else doesFit(element, phrases.tail)

  def contains (element: String, pattern: String): Boolean = {
    @tailrec
    def containsHelper(elementChars: List[Char], patternChars: List[Char]): Boolean = {
      if (elementChars == Nil && patternChars != Nil) false
      else if (patternChars == Nil) true
      else if (elementChars.head == patternChars.head) containsHelper(elementChars.tail, patternChars.tail)
      else containsHelper(elementChars.tail, patternChars)
    }
    containsHelper(element.toList, pattern.toList)
  }
}

