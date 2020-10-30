import scala.annotation.tailrec

object Task4 {

  @tailrec
  def isPrefix(word: String, prefix: String): Boolean = {
    (word, prefix) match {
      case (_, "") => true
      case ("", _) => false
      case (_, _) =>
        if (word.head == prefix.head) isPrefix(word.tail, prefix.tail)
        else false
    }
  }

  def stringContains(word: String, key: String): Boolean = {
    (word, key) match {
      case (_, "") => true
      case ("", _) => false
      case (_, _) =>
        if (isPrefix(word, key)) true
        else stringContains(word.tail, key)
    }
  }

  def findMatchOneRec(list: List[String], pattern: String): List[String] = {
    list match {
      case Nil => Nil
      case h::t =>
        if(stringContains(h, pattern)) h::findMatchOneRec(t, pattern)
        else findMatchOneRec(t, pattern)
    }
  }

  def findMatchOne(list: List[String], pattern: String): List[String] = {
    @tailrec
    def findMatchOneTail(list: List[String], pattern: String, result: List[String]): List[String] = {
      list match {
        case Nil => Lists.listReverse(result)
        case h::t =>
          if(stringContains(h, pattern)) findMatchOneTail(t, pattern, h::result)
          else findMatchOneTail(t, pattern, result)
      }
    }
    findMatchOneTail(list, pattern, Nil)
  }
}
