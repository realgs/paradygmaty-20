package functions

import utilities.UtilityFunctions._

import scala.annotation.tailrec

object ExerciseFour {
  def find(list: List[String], phrase: String): List[String] = {
    list match {
      case Nil => Nil
      case head :: tail =>
        if (contains(head, phrase))
          head :: find(tail, phrase)
        else
          find(tail, phrase)
    }
  }

  def findWithTailRecursion(list: List[String], phrase: String): List[String] = {
    @tailrec
    def findTail(list: List[String], accum: List[String]): List[String] =
      list match {
        case Nil => accum
        case head :: tail =>
          if (contains(head, phrase))
            findTail(tail, head :: accum)
          else
            findTail(tail, accum)
      }
    findTail(reverseList(list), Nil)
  }

  def findElementsContainingAtLeastOnePhrase(list: List[String], phrases: List[String]): List[String] = {
    (list, phrases) match {
      case (Nil, _) | (_, Nil) => Nil
      case (listHead :: listTail, _) =>
        if (containsAtLeastOne(listHead, phrases)) listHead :: findElementsContainingAtLeastOnePhrase(listTail, phrases)
        else findElementsContainingAtLeastOnePhrase(listTail, phrases)
    }
  }

  def findElementsContainingAtLeastOnePhraseWithTailRecursion(list: List[String], phrases: List[String]): List[String] = {
    @tailrec
    def inner(lista: List[String], frazy: List[String], currentResult: List[String]): List[String] = {
      (lista, frazy) match {
        case (Nil, _) | (_, Nil) => currentResult
        case (listHead :: listTail, _) =>
          if (containsAtLeastOne(listHead, phrases)) inner(listTail, phrases, listHead :: currentResult)
          else inner(listTail, phrases, currentResult)
      }
    }
    inner(reverseList(list), phrases, Nil)
  }

}
