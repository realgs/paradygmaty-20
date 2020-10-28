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

  @tailrec
  def matches(phrase: String, words: List[String]): Boolean = {
    words match {
      case Nil => false
      case head :: tail => if (contains(phrase, head)) true else matches(phrase, tail)
    }
  }

  @tailrec
  def contains(phrase: String, word: String): Boolean = {
    (phrase, word) match {
      case (_, "") => true
      case ("", _) => false
      case _ => if (phrase.head != word.head) false else contains(phrase.tail, word.tail)
    }
  }
}
