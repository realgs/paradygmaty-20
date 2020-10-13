object zad02 {
  def concat(list: List[String], separator: String, endChar: Char): String = {
    if (list == Nil) ""
    else if (list.tail == Nil) list.head + endChar
    else list.head + separator + concat(list.tail, separator, endChar)
  }

  def runTests(): Unit = {
    assert(concat(List("And", "so", "it", "was.."), " ", '.') == "And so it was...")
    assert(concat(List(), "???", '?') == "")
    assert(concat(List("sesh"), "???", '?') == "sesh?")
    assert(concat(List("", "", "", "", ""), ")", ':') == ")))):")
  }
}
