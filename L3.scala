//zadanie 1

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



def contains(elem: String, key: String) = {
  def contains_inner(elem: String, key: String, fullKey: String): Boolean =  //current character: elem.head, key.head
    if (elem.tail == "" && key.tail != "") false
    else if (key.tail == "" && key.head != 0 && elem.head == key.head) true  //ostatni z key
    else if (key.head == elem.head) contains_inner(elem.tail, key.tail, key)
    else contains_inner(elem.tail, fullKey, fullKey)
  contains_inner(elem, key, key)
}

def find[T](list: List[T], listOfKeys: List[T]) = {
  def findByOneKey[T](list2: List[T], key: T): List[String] =
    if (list2 == Nil) Nil
    else if (contains(list2.head.toString, key.toString)) list2.head.toString :: findByOneKey(list2.tail, key)
    else findByOneKey(list2.tail, key)

  def findingByAllKeysLoop[T](list3: List[T], listOfKeys: List[T]): List[List[String]] =
    if (listOfKeys == Nil) Nil
    else findByOneKey(list3, listOfKeys.head) :: findingByAllKeysLoop(list3, listOfKeys.tail)

  def listContains(list: List[String], key: String): Boolean =
    if (list == Nil) false
    else list.head == key || listContains(list.tail, key)

  def mergeAndDeleteRepeats(list4: List[List[String]]): List[String] = {
    if (list4 == Nil) list5
    else if (list5 == Nil) list4
    else if (!listContains(list5, list4.head)) list4.head :: mergeAndDeleteRepeats(list4.tail, list5)
    else mergeAndDeleteRepeats(list4.tail, list5)
  }

  findingByAllKeysLoop(list, listOfKeys)

}

find(List("ala", "alicja", "alina"), List("ali", "al"))
find(List("456", "4567", "4588"), List("56"))



contains("s", "tra") == false
contains("trata", "tra") == true
contains("456", "567") == false
contains("456", "56") == true
contains("456", "4") == true
contains("alicja", "alicja") == true
contains("alicja", "lic") == true
contains("alicja", "lic9") == false
contains("12345", "239") == false
contains("zamek", "ame") == true
contains("zamek", "ameh") == false