// own list.reverse
def listRev[A](list:List[A]):List[A]= {
  def help(list:List[A],reversed:List[A]):List[A] = list match{
    case Nil => reversed
    case head::tail => help(tail,head::reversed)
    }
  help(list,Nil)
}

//1
/*
zlozonosc:
obliczeniowa:O(n), n-dlugosc podanej listy
pamieciowa:O(1),
*/

def checkNegative(n:Int):Boolean = {
  n < 0
}

def checkOddNegative(n:Int):Boolean = {
  (n < 0) && ((n%2)!=0 )
}


def splitList(list:List[Int]):(List[Int],List[Int]) = {
  def help(list:List[Int],neg:List[Int],oddNeg:List[Int]):(List[Int],List[Int]) =list match {
  case Nil => (listRev(neg),listRev(oddNeg))
  case head::tail=>
    if (checkOddNegative(head)) help(tail,head::neg,head::oddNeg)
    else if (checkNegative(head)) help(tail,head::neg,oddNeg)
    else  help(list.tail,neg,oddNeg)
  }
  help(list,Nil,Nil)
}
// 1 test
splitList(List(-1,-2,-3,-4))
splitList(List(1,-2,-3,4))
splitList(List(1,2,3,4))
splitList(List(0))
splitList(List())


//2
/*
zlozonosc:
obliczeniowa:O(n), n-dlugosc podanej listy
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
obliczeniowa:O(n), n-dlugosc dluzszej listy
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

//5
// normal rec
zlozonosc:
obliczeniowa:O(n), n-suma dlugosci list
pamieciowa:O(n), n-suma dlugosci list
*/
def joinLists[A](a: List[A], b: List[A], c: List[A]): List[A] = (a, b, c) match{
    case(Nil, Nil, Nil) => Nil
    case(ha::ta, _, _) => ha::joinLists(ta, b, c)
    case(Nil, hb::tb, _)  => hb::joinLists(Nil, tb, c)
    case(Nil, Nil, hc::tc) => hc::joinLists(Nil, Nil, tc)
  }
// tail rec
/*
zlozonosc:
obliczeniowa:O(n), n-suma dlugosci list
pamieciowa:O(1),
*/
def joinListsT[A](a:List[A],b:List[A],c:List[A]):List[A] = {
  def help(a:List[A],b:List[A],c:List[A],acc:List[A]):List[A]= (a,b,c) match{
    case (Nil,Nil,Nil) => listRev(acc)
    case (ha::ta,_,_) => help(ta,b,c,ha::acc)
    case(Nil, hb::tb, _)  => help(Nil, tb, c, hb::acc)
    case(Nil, Nil, hc::tc) => help(Nil, Nil, tc, hc::acc)
  }
  help(a,b,c,Nil)
}
// 5 test
joinListsT(List(1,2),List(3,4),List(5,6))
joinListsT(List(1,2),List(),List(5,6))
joinListsT(List(1,2),List(3,4),List())
joinListsT(List(),List(3,4),List(5,6))
joinListsT(List(),List(),List())