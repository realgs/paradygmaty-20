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

def polacz(list1:List[Int],list2:List[Int]):List[Int] = {
  @tailrec
  def polaczHelper(list1: List[Int], list2: List[Int], accum: List[Int], counter:Int):List[Int] =
    (list1,list2) match {
      case (Nil, Nil) => accum
      case (Nil, h::t) => polaczHelper(Nil,t,h::accum,counter+1)
      case (h::t, Nil) => polaczHelper(t,Nil,h::accum,counter+1)
      case (h :: t, h2 :: t2) => if (counter % 2 == 0) polaczHelper(t, list2, h :: accum, counter + 1)
                                 else polaczHelper(list1, t2, h2 :: accum, counter + 1)
    }
  myReverse(polaczHelper(list1,list2,List(),0))
}

polacz(List(5,4,3,2),List(1,2,3,4,5,6))
polacz(List(),List(1,2,3,4,5,6))
polacz(List(5,4,3,2),List())
polacz(List(),List())
polacz(List(10,12,13,14,15,16,17,18),List(1,2,3,4,5,6))
