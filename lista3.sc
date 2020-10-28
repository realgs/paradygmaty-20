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

//Zadanie 3
def merge[A](firstList: List[A])(secondList: List[A]):List[A] =
{
  @scala.annotation.tailrec
  def mergeRec(accum: List[A])(list1: List[A])(list2: List[A]):List[A] =
    {
      (list1,list2) match
      {
        case (head1::tail1,head2::tail2) => mergeRec(head2::head1::accum)(tail1)(tail2)
        case (Nil,head::tail) => mergeRec(head::accum)(Nil)(tail)
        case (head::tail,Nil) => mergeRec(head::accum)(tail)(Nil)
        case _ => reverse(accum)
      }
    }
  mergeRec(List())(firstList)(secondList)
}

merge(List())(List()) == List()
merge(List('a','b'))(List('c','d','e','f')) == List('a','c','b','d','e','f')
merge(List(1,2,3,4))(List()) == List(1,2,3,4)
merge(List("Lorem","ipsum"))(List("dolor","sit")) == List("Lorem","dolor","ipsum","sit")
//Złożoność czasowa 0(n), złożoność pamięciowa 0(n)