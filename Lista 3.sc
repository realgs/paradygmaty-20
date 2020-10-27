def reverse[A](list: List[A]): List[A] =
{
  @scala.annotation.tailrec
  def reverseIter(list: List[A], reverse: List[A]): List[A] =
  {
    if(list == Nil) reverse
    else reverseIter(list.tail, list.head :: reverse)
  }
  reverseIter(list, List())
}

reverse(List(1, 2, 3, 4)) == List(4, 3, 2, 1)
reverse(List()) == List()

// Zadanie 1
def splitNegative (list: List[Int]): (List[Int], List[Int]) =
{
  def split(original: List[Int], first: List[Int], second: List[Int]): (List[Int], List[Int]) =
  {
    if(original == Nil) (first, second)
    else if (original.head < 0)
    {
      if(original.head % 2 != 0)
        split(original.tail, original.head :: first, original.head :: second)
      else
        split(original.tail, original.head :: first, second)
    }
    else
      split(original.tail, first, second)
  }
  val (first, second) = split(list, List(), List())
  (reverse(first), reverse(second))
}

splitNegative(List(-3, -6, 8, -9, 13)) == (List(-3, -6, -9), List(-3, -9))
splitNegative(List(-1, -2, -3, -4, -5, -6)) == (List(-1, -2, -3, -4, -5, -6), List(-1, -3, -5))
splitNegative(List(1, 2, 3, 4)) == (List(), List())

// Zadanie 2
def listLength [A](xs: List[A]): Int =
{
  @scala.annotation.tailrec
  def count(xs: List[A], length: Int): Int =
  {
    if(xs == Nil) length
    else count(xs.tail, length + 1)
  }
  count(xs, 0)
}

// Złożoność obliczeniowa: liniowa względem długości listy
// Złożoność pamięciowa: stała

listLength(List(5, 4, 3, 2)) == 4
listLength(List()) == 0
listLength(List(List(1, 2, 3), List(1, 2, 3, 4))) == 2
listLength(List("1", "2")) == 2

// Zadanie 3
def connect [A](first: List[A], second: List[A]): List[A] =
{
  @scala.annotation.tailrec
  def connectIter(connected: List[A], first: List[A], second: List[A]): List[A] =
  {
    (connected, first, second) match
    {
      case (connected, Nil, Nil) => connected
      case (connected, first, Nil) => reverse(connected) ::: first
      case (connected, Nil, second) => reverse(connected) ::: second
      case (connected, first, second) =>
        connectIter(second.head :: first.head :: connected, first.tail, second.tail)
    }
  }
  connectIter(List(), first, second)
}

// Złożoność obliczeniowa pesymistyczna: O(n + m) gdzie n to długość pierwszej listy, a m to długość drugiej listy
// Złożoność pamięciowa: stała

connect(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)
connect(List(1, 3, 5, 7), List(2, 4, 6, 8, 9, 10, 11)) == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
connect(List(), List(1, 2, 3)) == List(1, 2, 3)
connect(List(1, 2, 3), List()) == List(1, 2, 3)

// Zadanie 4
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
    if(list == Nil) reverse(result)
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
    if(list == Nil) reverse(result)
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

// Zadanie 5
def joinLists[A](first: List[A], second: List[A], third: List[A]): List[A] =
{
  (first, second, third) match
  {
    case (h :: t, _, _) => h :: joinLists(t, second, third)
    case (Nil, h :: t, _) => h :: joinLists(Nil, t, third)
    case (Nil, Nil, third) => third
  }
}

// Złożoność obliczeniowa i pamięciowa: liniowa względem sumy długości pierwszej i drugiej listy

joinLists(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9)
joinLists(List(), List(1), List(1, 2, 3)) == List(1, 1, 2, 3)
joinLists(List(1, 2, 3), List(), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6)
joinLists(List(), List(), List(1, 2, 3)) == List(1, 2, 3)

def joinListsTail[A](first: List[A], second: List[A], third: List[A]): List[A] =
{
  @scala.annotation.tailrec
  def joinListsIter(first: List[A], second: List[A], third: List[A], result: List[A])
  : List[A] =
  {
    (first, second, third, result) match
    {
      case (h :: t, _, _, result) => joinListsIter(t, second, third, h :: result)
      case (Nil, h :: t, _, result) => joinListsIter(Nil, t, third, h :: result)
      case (Nil, Nil, h :: t, result) => joinListsIter(Nil, Nil, t, h :: result)
      case (Nil, Nil, Nil, result) => result.reverse
    }
  }
  joinListsIter(first, second, third, List())
}

// Złożoność obliczeniowa: liniowa względem długości wszystkich trzech list
// Złożoność pamięciowa: stała

joinListsTail(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9)
joinListsTail(List(), List(1), List(1, 2, 3)) == List(1, 1, 2, 3)
joinListsTail(List(1, 2, 3), List(), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6)
joinListsTail(List(), List(), List(1, 2, 3)) == List(1, 2, 3)