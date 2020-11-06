object l2 {

  //ZADANIE 1
  def multiplyRec(xs:List[Double]): Double =
    if (xs==Nil) 1
    else xs.head*multiplyRec(xs.tail)

  def multiply(xs:List[Double]): Double =
    if(xs==Nil) 0
    else xs.head*multiplyRec(xs.tail)


  //ZADANIE 2
  def intoSentenceRec(xs:List[String], end:Char, separator:Char): String =
    if(xs==Nil) end+""
    else separator+xs.head+intoSentenceRec(xs.tail, end, separator)

  def intoSentence(xs:List[String], end:Char, separator:Char): String =
    if (xs==Nil) ""
    else xs.head+intoSentenceRec(xs.tail, end, separator)


  //ZADANIE 3
  def isBetween(xs:List[Double], lower:Double, higher:Double): Boolean =
    if (xs==Nil) true
    else if (xs.head>=lower && xs.head<=higher) isBetween(xs.tail, lower, higher)
    else false


  //ZADANIE 4
  def power(x:Double, y:Int): Double =
    if (y==0) 1
    else if (y>0) x*power(x, y-1)
    else (1/x)*power(x, y+1)

  //W zadaniu 1 i 2 główne metody wykonywujące się rekurencyjnie "opakowane" zostały
  //w dodatkowe metody obsługujące tylko pierwszy krok (xs.head), aby dla przypadków skrajnych
  //(podanie pustej listy) metody zwracały wynik, moim zdaniem, intuicyjny
}
