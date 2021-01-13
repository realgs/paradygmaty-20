//Zadanie 1. (2.5pkt)
def repeat(xs: List[Int],ys: List[Int]): List[Int]={
  if(xs.isEmpty||ys.isEmpty) List()
  else (xs.head,ys.head) match
  {
    case (_,0)=>repeat(xs.tail,ys.tail)
    case (x,y)=>x::repeat(xs,(y-1)::ys.tail)
  }
}

repeat(List(1,2,3),List(0,3,1,4))==List(2,2,2,3)
repeat(List(1,2,3,2),List(0,1,3,2))==List(2,3,3,3,2,2)
repeat(List(1,2,3,4),List(0,1,3))==List(2,3,3,3)

//Zadanie 2. (2.5pkt)
def removeDuplicate[T](elements:List[T]):List[T] = elements match {
  case Nil => elements
  case head::tail => head :: removeDuplicate(tail filterNot (_==head))
}

removeDuplicate(repeat(List(1,2,3),List(0,3,1,4)))
removeDuplicate(repeat(List(1,2,3,2),List(0,1,3,2)))
removeDuplicate(repeat(List(1,2,3,4),List(0,1,3)))