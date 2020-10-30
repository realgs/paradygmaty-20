import scala.annotation.tailrec

def append[A](list1:List[A], list2:List[A]):List[A] =
  list1 match {
    case Nil => list2
    case h :: t => h :: append(t,list2)
  }

def stringMatch(toCompare:String, pattern:String):Boolean = {
  if (toCompare.length < pattern.length || ((toCompare.length > pattern.length) && pattern.length == 0)) false
  else if (toCompare.length == 0 && pattern.length == 0) true
  else {
    @tailrec
    def insideString(toCompare: String, pattern: String, currIndex: Int, wordIndex:Int): Boolean = {
      if (currIndex == pattern.length - 1 && toCompare.charAt(wordIndex) == pattern.charAt(currIndex)) true
      else if (wordIndex >= toCompare.length - 1) false
      else if (toCompare.charAt(wordIndex) != pattern.charAt(currIndex) && currIndex > 0) insideString(toCompare, pattern,0, wordIndex)
      else if (toCompare.charAt(wordIndex) != pattern.charAt(currIndex)) insideString(toCompare, pattern,0, wordIndex + 1)
      else if (toCompare.charAt(wordIndex) == pattern.charAt(currIndex)) insideString(toCompare, pattern, currIndex + 1, wordIndex + 1)
      else false
    }
    insideString(toCompare, pattern, 0,0)
  }
}

def find(inputList:List[String],elem:String):List[String] = {
  @tailrec
  def findHelper(inputList: List[String], elem: String, accum: List[String]): List[String] =
    inputList match {
      case Nil => accum
      case h :: t => if (stringMatch(h, elem)) findHelper(t, elem, h :: accum)
                     else findHelper(t, elem, accum)
    }
  findHelper(inputList, elem, List())
}

def find2(inputList:List[String], elem:String):List[String] =
  inputList match {
    case Nil => Nil
    case h :: t => if (stringMatch(h, elem)) h :: find2(t, elem)
                   else find2(t, elem)
  }

find(List("haha", "haha", "haha"),"ha")
find(List("haha", "haha", "haha"),"aha")
find(List("hahapatternwsrodku", "haha", "haha"),"patt")

find(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"),"index0168")
find(List("hehexd", "hehexd3", "nieto1234", "", "papa"),"hehexd")
find(List("hehexd", "hehexd3", "nieto1234", "", "papa"),"eto")
find(List(),"")
find(List(""),"")
find(List("haha", "dziwna", "lista"),"")
find(List("haha", "dziwna", "lista"),"nic")

find2(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"),"index0168")
find2(List("hehexd", "hehexd123", "nieto1234", "", "papa"),"hehexd")
find2(List("hehexd", "hehexd123", "nieto1234", "", "papa"),"eto")
find2(List(),"")
find2(List(""),"")
find2(List("haha", "dziwna", "lista"),"")
find2(List("haha", "dziwna", "lista"),"nic")

def findElems(inputList:List[String], elems:List[String]):List[String] = {
  @tailrec
  def findElemsHelper(inputList:List[String], elems:List[String], accum:List[String]):List[String] = {
    elems match {
      case Nil => accum
      case h :: t => findElemsHelper(inputList, t, append(accum, find(inputList, h)))
    }
  }
  findElemsHelper(inputList,elems,List())
}

findElems(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224", "index0223", "index557"), List("index0168", "index0169"))
findElems(List("hehexd", "hehexd123", "nieto1234", "", "papa"), List("hehexd", "eto", "nie"))
findElems(List(), List(""))
findElems(List(""), List("", "nie"))
findElems(List("haha", "dziwna", "lista"),List("", "dziw"))
findElems(List("haha", "haha", "haha"),List("ha", "aha"))
