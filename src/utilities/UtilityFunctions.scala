package utilities

import scala.annotation.tailrec

object UtilityFunctions {
  def reverseList[A](list: List[A]): List[A] = {
    @tailrec
    def reverseTail(list: List[A], accum: List[A]): List[A] = {
      list match {
        case Nil => accum
        case head :: tail => reverseTail(tail, head :: accum)
      }
    }

    reverseTail(list, Nil)
  }

  def contains(element: String, phrase: String): Boolean = {
    @tailrec
    def innerContains(element: String, phrase: String, startingPhrase: String): Boolean = {
      (element, phrase) match {
        case (_, "") => true
        case ("", _) => false
        case (_, _) =>
          if (element.head == phrase.head) innerContains(element.tail, phrase.tail, startingPhrase)
          else innerContains(element.tail, startingPhrase, startingPhrase)
      }
    }
    innerContains(element, phrase, phrase)
  }

  @tailrec
  def containsAtLeastOne(element: String, phrases: List[String]): Boolean = {
    phrases match {
      case Nil => false
      case phrasesHead :: phrasesTail => if (contains(element, phrasesHead)) true else containsAtLeastOne(element, phrasesTail)
    }
  }
  
}
