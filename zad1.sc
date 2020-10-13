def multiply (numList:List[Double]):Double =
  if (numList != Nil)
    if (numList.tail != Nil) numList.head * multiply(numList.tail)
    else numList.head
  else 0


multiply(List(2,2,2,2))
multiply(List(1.5,2.5,2.3,4.5))
multiply(List(0,2,4,6))
multiply(List(-1,-2,-3,-4,-5))
multiply(List())