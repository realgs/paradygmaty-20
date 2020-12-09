import scala.collection.IterableOnce.iterableOnceExtensionMethods
//Lista 5 Maciej Kopiński

//Zadanie 1

def duplicate[A](elems: List[A], quantity: List[Int]) : List[A]={
  (elems, quantity) match {
    case (Nil, Nil) => Nil
    case (Nil, _) => Nil
    case (l, Nil) => l
    case (h1::t1, h2::t2) => if(h2==0) duplicate(t1, t2) else h1::duplicate(elems, (h2-1)::t2)
  }
}

duplicate(List(1, 2, 3), List(0, 3, 1, 4))

//Zadanie 2

def removeDuplicates[A](list : List[A]) : List[A]={
  def innerRemove(seen : Set[A], l : List[A], accum : List[A]) : List[A]={
    l match {
      case Nil => accum.reverse
      case h::t => if(seen.contains(h)) innerRemove(seen, t, accum) else innerRemove(seen+h, t, h::accum)
    }
  }
  innerRemove(Set(), list, Nil)
}

def duplicateUniqueElems[A](elems: List[A], quantity: List[Int]) : List[A]={
  duplicate(removeDuplicates(elems), quantity)
}

removeDuplicates(List(1, 2, 3, 1))


duplicateUniqueElems(List(1, 2, 3, 1), List(0, 3, 1, 4))


