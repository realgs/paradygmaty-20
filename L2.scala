class L2 {

  //1) Funkcja, która przyjmuje listę liczb rzeczywistych, a zwraca ich iloczyn.

  def multiplyRealNumb (list: List[Double]): Double = {
    if(list == Nil) 0
    else {
      if (list.length == 1) list.head
      else list.head * multiplyRealNumb(list.tail)
    }
  }


  /*2) Funkcja przyjmująca listę stringów(np słów), znak kończący zdanie oraz separator. Na wyjściu zwraca string będący połączeniem stringów
   wejściowych oddzielonych od siebie separatorem i zakończony określonym znakiem (np. kropka, wykrzyknik, znak zapytania).*/

  def createSentence (listOfStrings: List[String], sentenceEnd: Char, separator: Char): String =
    if(listOfStrings == Nil) sentenceEnd.toString
    else if(listOfStrings.length == 1) listOfStrings.head + createSentence(listOfStrings.tail, sentenceEnd, separator)
    else listOfStrings.head + separator.toString + createSentence(listOfStrings.tail, sentenceEnd, separator)


  /*3) Funkcja sprawdzająca czy liczby z podanej na wejściu listy mieszczą się w
   przedziale domkniętym [X, Y]. Zwraca true jeśli tak, false w przeciwnym przypadku.*/

  def isInInterval (list: List[Double], lowerEndpoint: Double, upperEndpoint: Double): Boolean =
    if(lowerEndpoint > upperEndpoint) throw new Exception(s"Lower endpoint is greater than higher endpoint.")
    else if(list == Nil) true
    else if(list.head < lowerEndpoint || list.head > upperEndpoint) false
    else isInInterval(list.tail, lowerEndpoint, upperEndpoint)


  //4) Funkcja, która liczy potęgę z danej liczby(podstawy) o danym wykładniku (np X^Y).

  def exponentiate (base: Double, exponent: Int): Double = {
    val res: Double = 1
    if (exponent == 0) res
    else if(exponent >= 0) res * base * exponentiate(base, exponent-1)
    else exponentiate(1.0/base, scala.math.abs(exponent))
  }


}
