import scala.annotation.tailrec

def myReverse(inputList:List[Int]):List[Int] = {
  @tailrec
  def myReverseHelp(accum: List[Int], inputList: List[Int]): List[Int] =
    inputList match {
      case Nil => accum
      case h :: t => myReverseHelp(h :: accum, t)
    }
  myReverseHelp(Nil,inputList)
}

def joinLists(list1:List[Int], list2:List[Int], list3:List[Int]): List[Int] =
    (list1,list2,list3) match {
      case (h :: t, _, _) => h :: joinLists(t, list2, list3)
      case (Nil, h :: t, _) => h :: joinLists(Nil, t, list3)
      case (Nil, Nil, _) => list3
    }

def joinListsTail(list1:List[Int], list2:List[Int], list3:List[Int]): List[Int] = {
  @tailrec
  def joinListsTailHelper(list1: List[Int], list2: List[Int], list3: List[Int], accum: List[Int]): List[Int] =
    (list1,list2,list3) match {
      case (Nil,Nil,Nil) => accum
      case (h :: t, _, _) => joinListsTailHelper(t, list2, list3, h :: accum)
      case (Nil, h :: t, _) => joinListsTailHelper(Nil, t, list3, h :: accum)
      case (Nil, Nil, h :: t) => joinListsTailHelper(Nil, Nil, t, h :: accum)
    }
  myReverse(joinListsTailHelper(list1,list2,list3,List()))
}

joinLists(List(),List(),List())
joinLists(List(1,2,3),List(),List())
joinLists(List(),List(1,2,3),List())
joinLists(List(),List(),List(1,2,3))
joinLists(List(1,2,3),List(4,5,6),List())
joinLists(List(),List(1,2,3),List(4,5,6))
joinLists(List(1,2,3),List(),List(4,5,6))
joinLists(List(1,2,3),List(4,5,6),List(7,8,9))
joinLists(List(5,4,3,2),List(1,0),List(9))
joinListsTail(List(),List(),List())
joinListsTail(List(1,2,3),List(),List())
joinListsTail(List(),List(1,2,3),List())
joinListsTail(List(),List(),List(1,2,3))
joinListsTail(List(1,2,3),List(4,5,6),List())
joinListsTail(List(),List(1,2,3),List(4,5,6))
joinListsTail(List(1,2,3),List(),List(4,5,6))
joinListsTail(List(1,2,3),List(4,5,6),List(7,8,9))
joinListsTail(List(5,4,3,2),List(1,0),List(9))
