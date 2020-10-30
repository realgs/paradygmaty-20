//2
/*
zlozonosc:
obliczeniowa:O(n), n-długosc podanej listy
pamieciowa:O(1),
*/
def listLenght[A](list:List[A]):Int = {
  def help(acc:Int,list:List[A]):Int = list match {
    case Nil => acc
    case head::tail => help(acc+1,list.tail)
  }
  help(0,list)
}
//2 test
listLenght(List())
listLenght(List(1))
listLenght(List(1,2))
listLenght(List('a','b','c'))

//3
/*
zlozonosc:
obliczeniowa:O(n), n-długosc dłuższej listy
pamieciowa:O(1),
*/
def mergeLists[A](listA:List[A],listB:List[A]):List[A] = (listA,listB) match {
  case (Nil,Nil) => Nil
  case (listA,Nil) => listA
  case (Nil,listB) => listB
  case (headA::tailA,headB::tailB) => headA::headB::mergeLists(tailA,tailB)
}
//3 test
mergeLists(List(1,3,5,7),List(2,4,6,8))
mergeLists(List(1,3,5,7),List(2,4))
mergeLists(List(1,3),List(2,4,6,8))
mergeLists(List(1,3),List('a','b'))
mergeLists(List(1,3),List())
mergeLists(List(),List(1))
mergeLists(List(),List())
