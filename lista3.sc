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

def createList(from:Int)(to:Int):List[Int] =
  {
    @scala.annotation.tailrec
    def createListRec(current:Int)(accum:List[Int]):List[Int] =
      if(current <= to) createListRec(current + 1)(current::accum)
      else reverse(accum)

    createListRec(from)(List())
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

//Zadanie 5
//Zwykła rekurencja
def joinLists[A](firstList : List[A])(secondList : List[A])(thirdList: List[A]):List[A] =
{
  if(firstList != Nil) firstList.head::joinLists(firstList.tail)(secondList)(thirdList)
  else if(secondList != Nil) secondList.head::joinLists(firstList)(secondList.tail)(thirdList)
  else if(thirdList != Nil) thirdList.head::joinLists(firstList)(secondList)(thirdList.tail)
  else Nil
}

joinLists(List())(List())(List()) == List()
joinLists(List(1,2,3,4))(List(5,6))(List(7,8,9,10,11)) == List(1,2,3,4,5,6,7,8,9,10,11)
joinLists(List())(List())(List(List())) == List(List())
joinLists(List('a','b'))(List('c'))(List('d','e','f')) == List('a','b','c','d','e','f')
//StackOverflowError
//joinLists(createList(0)(50000))(createList(50001)(100000))(createList(100001)(150000)) == createList(0)(150000)
//Rekurencja ogonowa
def joinListsTail[A](firstList: List[A])(secondList: List[A])(thirdList: List[A]):List[A] =
{
   @scala.annotation.tailrec
   def joinListsRec(accum: List[A])(list1: List[A])(list2: List[A])(list3: List[A]):List[A] =
   {
     if(list1 != Nil) joinListsRec(list1.head::accum)(list1.tail)(list2)(list3)
     else if(list2 != Nil) joinListsRec(list2.head::accum)(list1)(list2.tail)(list3)
     else if(list3 != Nil) joinListsRec(list3.head::accum)(list1)(list2)(list3.tail)
     else reverse(accum)
   }
   joinListsRec(List())(firstList)(secondList)(thirdList)
}

joinListsTail(List())(List())(List()) == List()
joinListsTail(List(1,2,3,4))(List(5,6))(List(7,8,9,10,11)) == List(1,2,3,4,5,6,7,8,9,10,11)
joinListsTail(List())(List())(List(List())) == List(List())
joinListsTail(List('a','b'))(List('c'))(List('d','e','f')) == List('a','b','c','d','e','f')
joinListsTail(createList(0)(50000))(createList(50001)(100000))(createList(100001)(150000)) == createList(0)(150000)