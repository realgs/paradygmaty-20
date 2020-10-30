import scala.annotation.tailrec

@tailrec
def contains[A](list1:List[A],elem:A):Boolean =
  list1 match {
    case Nil => false
    case h :: t  => if (h == elem) true else contains(t,elem)
  }

def appendNoRep[A](list1:List[A], list2:List[A]):List[A] = {
  @tailrec
  def appendNoRepHelper[A](list1:List[A], list2:List[A], accum:List[A]):List[A] =
    list2 match {
      case Nil => accum
      case h :: t => if (contains(list1,h)) appendNoRepHelper(list1, t, accum) else appendNoRepHelper(list1, t, h :: accum)
    }
  appendNoRepHelper(list1,list2,list1)
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
      case h :: t => findElemsHelper(inputList, t, appendNoRep(accum, find(inputList, h)))
    }
  }
  findElemsHelper(inputList,elems,List())
}

findElems(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224", "index0223", "index557"), List("index0168", "index0169"))
findElems(List("hehexd", "hehexd123", "nieto1234", "", "papa"), List("hehexd", "eto", "nie","ehe"))
findElems(List(), List(""))
findElems(List(""), List("", "nie"))
findElems(List("haha", "dziwna", "lista"),List("", "dziw"))
findElems(List("haha", "haha", "haha"),List("ha", "aha"))
findElems(List("TuSprawdzenie", "NaPatterny", "WSrodku"),List("rawd", "tt"))

/*ODPOWIEDŹ DO PORÓWNANIA REKURSJI W ZAD4,5
  Porównanie funkcji w rekurencji ogonowej i nieogonowej skutkuje: w ogonowej przyspieszenie szybkości działania
  poprzez pracę w jednym miejscu, w nieogonowej mamy znacznie dłuższy czas działania. Powodem są tworzone
  nowe wywołania na stosie, podczas gdy w rekursji ogonowej one nie występują
 */

