def strContains(left: String, right: String): Boolean =
{
  if(left == "" && right == "") return true
  else if(left == "" || right == "") return false
  @scala.annotation.tailrec
  def strContainsIter(left: String, rightCopy: String): Boolean =
  {
    if (left.isEmpty) false
    else if (rightCopy.isEmpty) true
    else if (left.head == rightCopy.head)
    {
      if(left.tail == "" && rightCopy.tail == "")
        true
      else
        strContainsIter(left.tail, rightCopy.tail)
    }
    else strContainsIter(left.tail, right)
  }
  strContainsIter(left, right)
}

// Złożoność obliczeniowa: O(n) gdzie n - rozmiar Stringa w którym funkcja poszukuje innego Stringa
// Złożoność pamięciowa: stała

@scala.annotation.tailrec
def containsForEach(string: String, phrases: List[String]): Boolean =
{
  if(phrases == Nil) false
  else if(strContains(string, phrases.head)) true
  else containsForEach(string, phrases.tail)
}

strContains("Alamakota", "Ala") == true
strContains("Alamakota", "ma") == true
strContains("Alamakota", "kota") == true
strContains("Alamakota", "Alamakota") == true
strContains("Alamakota", "m") == true
strContains("Alamakota", "psa") == false
strContains("Ala", "Alama") == false

def find(list: List[String], phrase: String): List[String] =
{
  if(list == Nil) Nil
  else if (strContains(list.head, phrase)) list.head :: find(list.tail, phrase)
  else find(list.tail, phrase)
}

// Złożoność obliczeniowa: O(k*n), gdzie n - średnia długość Stringa w liście (strContains),
// k - długość listy
// Złożoność pamięciowa: O(k), gdzie k - długość listy.

def findTail(list: List[String], phrase: String): List[String] =
{
  @scala.annotation.tailrec
  def findTailIter(list: List[String], result: List[String]): List[String] =
  {
    if(list == Nil) result.reverse
    else if (strContains(list.head, phrase)) findTailIter(list.tail, list.head :: result)
    else findTailIter(list.tail, result)
  }
  findTailIter(list, List())
}

// Złożoność obliczeniowa: O(k*n + l), gdzie n - średnia długość Stringa w liście (strContains),
// k - długość listy, l - długość listy wynikowej
// Złożoność pamięciowa: stała

find(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), "index0168") == List("index0168202", "index0168211", "index0168210")
find(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), "index0169") == List("index0169", "index0169222", "index0169224")
find(List("I", "am", "the", "orphan", "the", "one", "who", "kills"), "the") == List("the", "the")
find(List("stream", "string", "stun"), "str") == List("stream", "string")
find(List("Gerwart", "Gerald", "Geralt"), "Geraltt") == List()

findTail(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), "index0168") == List("index0168202", "index0168211", "index0168210")
findTail(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), "index0169") == List("index0169", "index0169222", "index0169224")
findTail(List("I", "am", "the", "orphan", "the", "one", "who", "kills"), "the") == List("the", "the")
findTail(List("stream", "string", "stun"), "str") == List("stream", "string")
findTail(List("Gerwart", "Gerald", "Geralt"), "Geraltt") == List()

def findMultiple(list: List[String], phrases: List[String]): List[String] =
{
  if(list == Nil) Nil
  else if (containsForEach(list.head, phrases)) list.head :: findMultiple(list.tail, phrases)
  else findMultiple(list.tail, phrases)
}

// Złożoność obliczeniowa: O(k*n*m), gdzie k - długość listy początkowej, n - średnia długość
// Stringa w liście, m - długość listy fraz do wyszukania
// Złożoność pamięciowa - O(k) gdzie k - długość listy

def findMultipleTail(list: List[String], phrases: List[String]): List[String] =
{
  @scala.annotation.tailrec
  def findMultipleIter(list: List[String], result: List[String]): List[String] =
  {
    if(list == Nil) result.reverse
    else if (containsForEach(list.head, phrases))
      findMultipleIter(list.tail, list.head :: result)
    else findMultipleIter(list.tail, result)
  }
  findMultipleIter(list, List())
}

// Złożoność obliczeniowa: O(k*n*m + l), gdzie k - długość listy początkowej, n - średnia długość
// Stringa w liście, m - długość listy fraz do wyszukania, l - długość listy wynikowej
// Złożoność pamięciowa - stała

findMultiple(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), List("index0168", "index0169")) == List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224")
findMultiple(List("I", "am", "the", "orphan", "the", "one", "who", "kills"), List("the", "o")) == List("the", "orphan", "the", "one", "who")
findMultiple(List("Prawdopodobnie", "ta", "funkcja", "dziala"), List("Prawdo", "podobnie", "fun")) == List("Prawdopodobnie", "funkcja")
findMultiple(List("0123456789", "1234", "2137", "1410", "1444", "123445"), List("1234", "44")) == List("0123456789", "1234", "1444", "123445")

findMultipleTail(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), List("index0168", "index0169")) == List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224")
findMultipleTail(List("I", "am", "the", "orphan", "the", "one", "who", "kills"), List("the", "o")) == List("the", "orphan", "the", "one", "who")
findMultipleTail(List("Prawdopodobnie", "ta", "funkcja", "dziala"), List("Prawdo", "podobnie", "fun")) == List("Prawdopodobnie", "funkcja")
findMultipleTail(List("0123456789", "1234", "2137", "1410", "1444", "123445"), List("1234", "44")) == List("0123456789", "1234", "1444", "123445")
