//Jakub Kochański
def reverse[A](list: List[A]):List[A] =
{
  def reverseRec[A](accumulator: List[A])(newList: List[A]):List[A] =
  {
    if(newList == Nil) accumulator
    else reverseRec(newList.head::accumulator)(newList.tail)
  }
  reverseRec(List())(list)
}

reverse(List(1,2,3,4,5))
//Zadanie 1
val splitList:List[Int] => (List[Int],List[Int]) = listToSplit =>
{
  def splitListRec: List[Int] => (List[Int],List[Int]) => (List[Int],List[Int]) = newList => (first,second) =>
    if(newList == Nil) (first,second)
    else if(newList.head % 2 == -1) splitListRec(newList.tail)(newList.head::first,newList.head::second)
    else if(newList.head < 0) splitListRec(newList.tail)(newList.head::first,second)
    else splitListRec(newList.tail)(first,second)
  splitListRec( reverse(listToSplit))(List(),List())
}

splitList(List()) == (List(),List())
splitList(List(1,2,3)) == (List(),List())
splitList(List(-2,-4,-6,-8)) == (List(-2,-4,-6,-8),List())
splitList(List(-1,-3,-5,-7)) == (List(-1,-3,-5,-7),List(-1,-3,-5,-7))
splitList(List(-1,-2,-3,-4,-5,-6,-7,-8)) == (List(-1,-2,-3,-4,-5,-6,-7,-8),List(-1,-3,-5,-7))
splitList(List(-1,2,-3,-4,5,6,7,-9)) == (List(-1,-3,-4,-9),List(-1,-3,-9))

//Zadanie 2
def listLength[A](list: List[A]):Int =
  if(list == Nil) 0
  else 1 + listLength(list.tail)

listLength(List()) == 0
listLength(List(1)) == 1
listLength(List(List(),List(),List(List()))) == 3
listLength(List('a','b','c','d','e','f')) == 6
//Złożoność czasowa O(n), pamięciowa 0(1)