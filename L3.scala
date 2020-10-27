//zadanie 1
def listDivision(list: List[Int]): (List[Int], List[Int]) = {
  def firstCrit(arg_list: List[Int]): List[Int] =
    if (arg_list == Nil) Nil
    else if (arg_list.head < 0) arg_list.head :: firstCrit(arg_list.tail)
    else firstCrit (arg_list.tail)
  def secondCrit(arg_list: List[Int]): List[Int] =
    if (arg_list == Nil) Nil
    else if (arg_list.head < 0 && arg_list.head % 2 != 0) arg_list.head :: secondCrit(arg_list.tail)
    else secondCrit(arg_list.tail)

  if (list == Nil) (Nil,Nil) else (firstCrit(list), secondCrit(list))

}

listDivision(List(-3, -6, 8, -9, 13)) == (List(-3, -6, -9),List(-3, -9))
listDivision(List()) == (Nil, Nil)
listDivision(List(0, 1, 2, 3, 4, 5)) == (Nil, Nil)
listDivision(List(0, -1)) == (List(-1), List(-1))
listDivision(List(0, -1)) == (List(-1), List(-1))


//zadanie 2
def length[T](list : List[T]): Int =
  if (list == Nil) 0
  else 1 + length(list.tail)
//złożoność obliczeniowa O(n)
//złożoność pamięciowa - stos n wywołań

length(List(1,2,3,4,5)) == 5
length(List("ala", "to", "kot")) == 3
length(List(List(1,2,3), List("ala"))) == 2
length(List()) == 0
length(List(1.23, 1.67, 900)) == 3


//zadanie 3
def merge[T](list1: List[T], list2: List[T]): List[T] = {
  def innerMerge[T](list1: List[T], list2: List[T], turn: Int): List[T] =
    if (list1 == Nil) list2
    else if (list2 == Nil) list1
    else if (turn == 1) list1.head :: innerMerge(list1.tail, list2, 2)
    else list2.head :: innerMerge(list1, list2.tail, 1)

  innerMerge(list1, list2, 1)
}

merge(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)
merge(List("Ala", "kota", "go"), List("ma", "i", "lubi")) == List("Ala", "ma", "kota", "i", "go", "lubi")
merge(Nil, Nil) == Nil
merge(List(10, 20, 30, 40), List()) == List(10, 20 ,30 ,40)
merge(List(), List(10, 20, 30, 40)) == List(10, 20 ,30 ,40)
merge(List(4.5, 6.8), List(1.2, 20, 30, 40, 9.9)) == List(4.5, 1.2, 6.8, 20, 30, 40, 9.9)

    
//zadanie 5
def joinLists[T](list1: List[T], list2: List[T], list3: List[T]): List[T] = {

  //innerJoinLists complexity O(n), n - list1 length
  def innerJoinLists[T](list1: List[T], list2: List[T]): List[T] =
    if (list1 == Nil) list2
    else if (list2 == Nil) list1
    else list1.head :: innerJoinLists(list1.tail, list2)

  //total complexity O((n + m) + p)
  innerJoinLists(innerJoinLists(list1, list2), list3)
}

joinLists(List(1), List(2), List(3)) == List(1, 2, 3)
joinLists(List("Ala"), List("ma"), List("kota")) == List("Ala", "ma", "kota")
joinLists(List(), List(), List("test")) == List("test")
joinLists(List(), List(), List()) == List()
joinLists(List(2.5, 2.6), List(1.0), List()) == List(2.5, 2.6, 1.0)
joinLists(List(2.5, 2.6), List(1.0), List(90)) == List(2.5, 2.6, 1.0, 90)


//zadanie 5 - tail recursion
import scala.annotation.tailrec

def joinListsTail[T](list1: List[T], list2: List[T], list3: List[T]): List[T] = {

  // O(n), ale stos wywołań: 1, dzięki optymalizacji kompilatora
  @tailrec
  def innerJoinLists[T](list1: List[T], list2: List[T], acc: List[T] = Nil): List[T] =
    list1 match{
      case Nil => list2
      case head :: tail => innerJoinLists(tail, head::list2)
    }

  // O(n)
  @tailrec
  def reverse(list: List[T], acc: List[T] = Nil): List[T] =
    list match{
      case Nil => acc
      case head :: tail => reverse(tail, head::acc)
    }

  innerJoinLists(reverse(innerJoinLists(reverse(list1), list2)), list3)
}

joinListsTail(List(1), List(2), List(3)) == List(1, 2, 3)
joinListsTail(List("Ala"), List("ma"), List("kota")) == List("Ala", "ma", "kota")
joinListsTail(List(), List(), List("test")) == List("test")
joinListsTail(List(), List(), List()) == List()
joinListsTail(List(2.5, 2.6), List(1.0), List()) == List(2.5, 2.6, 1.0)
joinListsTail(List(2.5, 2.6), List(1.0), List(90)) == List(2.5, 2.6, 1.0, 90)


//zadanie 4
def find[T](list: List[T], listOfKeys: List[T]) = {
  def contains(elem: String, key: String) = {
    def contains_inner(elem: String, key: String, fullKey: String): Boolean =
      if (elem.tail == "" && key.tail != "") false
      else if (key.tail == "" && key.head != 0 && elem.head == key.head) true
      else if (key.head == elem.head) contains_inner(elem.tail, key.tail, key)
      else contains_inner(elem.tail, fullKey, fullKey)
    contains_inner(elem, key, key)
  }
    //  func contains() tests
//  contains("s", "tra") == false
//  contains("trata", "tra") == true
//  contains("456", "567") == false
//  contains("456", "56") == true
//  contains("456", "4") == true
//  contains("alicja", "alicja") == true
//  contains("alicja", "lic") == true
//  contains("alicja", "lic9") == false
//  contains("12345", "239") == false
//  contains("zamek", "ame") == true
//  contains("zamek", "ameh") == false

  def findByOneKey[T](list2: List[T], key: T): List[String] =
    if (list2 == Nil) Nil
    else if (contains(list2.head.toString, key.toString)) list2.head.toString :: findByOneKey(list2.tail, key)
    else findByOneKey(list2.tail, key)

  def findingByAllKeysLoop[T](list3: List[T], listOfKeys: List[T]): List[String] =
    if (listOfKeys == Nil) Nil
    else joinLists(findingByAllKeysLoop(list3, listOfKeys.tail), findByOneKey(list3, listOfKeys.head))

  def listContains(list: List[String], key: String): Boolean =
    if (list == Nil) false
    else list.head == key || listContains(list.tail, key)

  def deleteRepeats(list: List[String], result: List[String] = Nil): List[String] =
    list match{
      case Nil => result
      case head :: tail if listContains(result, head) => deleteRepeats(tail, result)
      case head :: tail => deleteRepeats(tail, head :: result)
    }

  def joinLists[T](list1: List[T], list2: List[T]): List[T] = {
    if (list1 == Nil) list2
    else if (list2 == Nil) list1
    else list1.head :: joinLists(list1.tail, list2)
  }

  deleteRepeats(findingByAllKeysLoop(list, listOfKeys))
}

find(List("ala", "alicja", "alina","oskar"), List("a")) == List("oskar", "alina", "alicja", "ala")
find(List(), List("a")) == List()
find(List("student6745", "student6666", "student9967"), List()) == List()
find(List("student6745", "student6666", "student9967"), List("67")) == List("student9967", "student6745")
find(List("student6745", "student6666", "student9967"), List("67","student67")) == List("student9967", "student6745")
find(List("student6745", "student6666", "student9967"), List("67","6")) == List("student9967", "student6666", "student6745")
find(List("student6745", "student6666", "student9967"), List("65","66")) == List("student6666")
find(List("student6745", "student6666", "student9967"), List("udent")) == List("student9967", "student6666", "student6745")
find(List("student6745", "student6666", "student9967"), List("u")) == List("student9967", "student6666", "student6745")
find(List("student6745", "student6666", "student9967"), List("s")) == List("student9967", "student6666", "student6745")
find(List("456", "4567", "4588"), List("56")) == List("4567", "456")
find(List("szafa", "szafka", "szafeczka", "kafka", "agrafka"), List("afka")) == List("agrafka", "kafka", "szafka")
find(List("szafa", "szafka", "szafeczka", "kafka", "agrafka","szuflada"),List("afka", "szaf", "lada"))  == List("agrafka", "kafka", "szafeczka", "szafka", "szafa", "szuflada")
find(List("szafa", "szafka", "szafeczka", "kafka", "agrafka", "szuflada"),List("flada", "samochód", "dom", "pies")) == List("szuflada")


//zadanie 4 tail recursion
def findTail[T](list: List[T], listOfKeys: List[T]) = {
  def contains(elem: String, key: String) = {
    def contains_inner(elem: String, key: String, fullKey: String): Boolean =
      if (elem.tail == "" && key.tail != "") false
      else if (key.tail == "" && key.head != 0 && elem.head == key.head) true
      else if (key.head == elem.head) contains_inner(elem.tail, key.tail, key)
      else contains_inner(elem.tail, fullKey, fullKey)
    contains_inner(elem, key, key)
  }

  @tailrec
  def findByOneKey[T](list2: List[T], key: T, acc: List[String] = Nil): List[String] =
    if (list2 == Nil) acc
    else if (contains(list2.head.toString, key.toString)) findByOneKey(list2.tail, key, list2.head.toString::acc)
    else findByOneKey(list2.tail, key, acc)

  def findingByAllKeysLoop[T](list3: List[T], listOfKeys: List[T]): List[String] =
    if (listOfKeys == Nil) Nil
    else joinLists(findingByAllKeysLoop(list3, listOfKeys.tail), findByOneKey(list3, listOfKeys.head))

  @tailrec
  def listContains(list: List[String], key: String, acc: Boolean = false): Boolean =
    if (list == Nil) acc
    else  listContains(list.tail, key, acc || list.head == key)

  @tailrec
  def deleteRepeats(list: List[String], result: List[String] = Nil): List[String] =
    list match{
      case Nil => result
      case head :: tail if listContains(result, head) => deleteRepeats(tail, result)
      case head :: tail => deleteRepeats(tail, head :: result)
    }

  @tailrec
  def joinLists[T](list1: List[T], list2: List[T], acc: List[T] = Nil): List[T] =
    list1 match{
      case Nil => list2
      case head :: tail => joinLists(tail, head::list2)
    }

  deleteRepeats(findingByAllKeysLoop(list, listOfKeys))
}

findTail(List("ala", "alicja", "alina","oskar"), List("a")) == List("ala", "alicja", "alina","oskar")
findTail(List(), List("a")) == List()
findTail(List("student6745", "student6666", "student9967"), List()) == List()
findTail(List("student6745", "student6666", "student9967"), List("67")) == List("student6745", "student9967")
findTail(List("student6745", "student6666", "student9967"), List("67","student67")) == List("student9967", "student6745")
findTail(List("student6745", "student6666", "student9967"), List("67","6")) == List("student9967", "student6666", "student6745")
findTail(List("student6745", "student6666", "student9967"), List("65","66")) == List("student6666")
findTail(List("student6745", "student6666", "student9967"), List("udent")) == List("student6745", "student6666", "student9967")
findTail(List("student6745", "student6666", "student9967"), List("u")) == List("student6745", "student6666", "student9967")
findTail(List("student6745", "student6666", "student9967"), List("s")) == List("student6745", "student6666", "student9967")
findTail(List("456", "4567", "4588"), List("56")) == List("456", "4567")
findTail(List("szafa", "szafka", "szafeczka", "kafka", "agrafka"), List("afka")) == List("szafka", "kafka", "agrafka")
findTail(List("szafa", "szafka", "szafeczka", "kafka", "agrafka", "szuflada"),List("afka", "szaf", "lada")) == List("kafka", "agrafka", "szuflada", "szafeczka", "szafka", "szafa")
findTail(List("szafa", "szafka", "szafeczka", "kafka", "agrafka", "szuflada"),List("flada", "samochód", "dom", "pies")) == List("szuflada")
