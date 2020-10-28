def mergeList[A](a : List[A], b : List[A], c : List[A]) : List[A] = {
  def appendList[A](a: List[A], b: List[A]): List[A] = {
    (a, b) match {
      case (_, Nil) => a
      case (Nil, _) => b
      case (head :: tail, _) => head :: appendList(tail, b)
    }
  }
  if(b == Nil) appendList(a,c)
  else {
    appendList(appendList(a,b),c)
  }
}
mergeList(List(5,4,3), List(2,1), List(9))
mergeList(List(5,4,3), List(), List(9))