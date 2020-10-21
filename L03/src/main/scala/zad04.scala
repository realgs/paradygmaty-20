import annotation.tailrec

object zad04 {
  def find(phrases: List[String], words: List[String]): List[String] = {
    phrases match {
      case Nil => Nil
      case head :: tail => if (matches(head, words)) head :: find(tail, words) else find(tail, words)
    }
  }

  def findTail(phrases: List[String], words: List[String]): List[String] = {
    @tailrec
    def findTailInternal(phrases: List[String], words: List[String], result: List[String]): List[String] = {
      phrases match {
        case Nil => result
        case head :: tail => findTailInternal(tail, words, if (matches(head, words)) head :: result else result)
      }
    }

    Utils.reverseList(findTailInternal(phrases, words, List()))
  }

  // TODO: write own implementation of string.contains
  @tailrec
  def matches(phrase: String, words: List[String]): Boolean = {
    words match {
      case Nil => false
      case head :: tail => if (phrase.contains(head)) true else matches(phrase, tail)
    }
  }
}
