def find(list: List[String], element: String): List[String] =
  list match {
    case Nil => Nil
    case head :: tail => if(head.contains(element)) head :: find(tail, element)
    else find(tail, element)
  }

find(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), "index0169")
find(List("a", "ab", "bd", "cab", "bad", "cd"), "a")