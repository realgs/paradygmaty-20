//zad1
def Split(list: List[Int]): (List[Int], List[Int]) = {
  def SplitHelp(list: List[Int], lists:(List[Int], List[Int])): (List[Int], List[Int])={
    if(list==Nil)
      return lists
    if(list.head<0) {
      if (list.head % 2 != 0)
        return SplitHelp(list.tail, (lists._1 :+ list.head, lists._2 :+ list.head))
      else
        return SplitHelp(list.tail, (lists._1 :+ list.head, lists._2))
    }
    return SplitHelp(list.tail,lists)
  }
  SplitHelp(list,(Nil,Nil))
}
Split(List(-3,-6,8,-9,13))
Split(Nil)
Split(List(-1,-3,-5,10))
//zad2
def ListLength[T](list: List[T]):Int = {
  if(list==Nil)
    return 0
  else
    return 1 + ListLength(list.tail)
}
//zlozonosc liniowa wzgledem dlugosci listy
ListLength(List(1,2,3,4,5))
ListLength((Nil))
ListLength(List("a","b","c","d"))
//zad3
def Combine[T](list1: List[T], list2: List[T]):List[T] = {
  def CombineHelp(list1: List[T], list2: List[T], output: List[T]):List[T] ={
    (list1,list2) match {
      case (_, Nil) => return output ::: list1
      case (Nil, _) => return output ::: list2
      case _ => return CombineHelp(list1.tail, list2.tail, output :+ list1.head :+ list2.head)
    }
  }
  return CombineHelp(list1,list2,Nil)
}
//zlozonosc liniowa wzgledem dlugosci krotszej z list
Combine(List(1,3,5,7),List(2,4,6))
Combine(List(1,3,5,7),List(2,4,6,8))
Combine(List(1,3,5,7),List(2,4,6,8,9,10,11,12,13))
//zad4
def FindElements(list: List[String], element: String):List[String] = {
  def FindElementsHelp(list: List[String], element: String, output: List[String]):List[String]={
    if(list==Nil)
      return output
    if(list.head.startsWith(element))
      return FindElementsHelp(list.tail ,element, output:+list.head)
    else
      return FindElementsHelp(list.tail,element,output)
  }
  FindElementsHelp(list,element,Nil)
}

FindElements(List("abcd","abcde","abcdef","gbhds"),"abc")
//zad5
def Combine3Lists[T](list1: List[T], list2: List[T], list3: List[T]):List[T] = {
  return list1:::list2:::list3
}

Combine3Lists(List(1,2,3),List(4,5,6),List(7,8,9))

