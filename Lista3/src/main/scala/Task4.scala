import scala.annotation.tailrec

object Task4 {

  //pomocnicze funkcje służące znajdowaniu wzorca w napisie
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

  @tailrec
  def stringContains(word: String, key: String): Boolean = {
    (word, key) match {
      case (_, "") => true
      case ("", _) => false
      case (_, _) =>
        if (isPrefix(word, key)) true
        else stringContains(word.tail, key)
    }
  }

  @tailrec
  def stringContainsAny(word: String, keys: List[String]): Boolean = {
    if (keys == Nil) false
    else if(stringContains(word, keys.head)) true
    else stringContainsAny(word, keys.tail)
  }

  //Zad 4 dla jednego szukanego elementu w wersjach rekursji i rekursji ogonowej
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

  //Zad 4 dla n szukanych elementów w wersjach rekursji i rekursji ogonowej
  def findMatchManyRec(list: List[String], patterns: List[String]): List[String] = {
    (list, patterns) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (h::t, _) =>
        if (stringContainsAny(h, patterns)) h::findMatchManyRec(t, patterns)
        else findMatchManyRec(t, patterns)
    }
  }

  def findMatchMany(list: List[String], patterns: List[String]): List[String] = {
    @tailrec
    def findMatchManyTail(list: List[String], patterns: List[String], result: List[String]): List[String] = {
      (list, patterns) match {
        case (Nil, _) => Lists.listReverse(result)
        case (_, Nil) => Lists.listReverse(result)
        case (h::t, _) =>
          if (stringContainsAny(h, patterns)) findMatchManyTail(t, patterns, h::result)
          else findMatchManyTail(t, patterns, result)
      }
    }
    findMatchManyTail(list, patterns, Nil)
  }
}
