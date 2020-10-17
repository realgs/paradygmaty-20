def range(numbers:List[Double],smallerNum:Double,biggerNum:Double):Boolean =
  if (smallerNum > biggerNum)
    range(numbers,biggerNum,smallerNum)
  else
    if (numbers != Nil)
      if (numbers.head > biggerNum || numbers.head < smallerNum) false
      else if (numbers.tail == Nil) true
      else range(numbers.tail,smallerNum,biggerNum)
    else false

range(List(1,4,6,7,15,23),0,22) == false
range(List(-30,4,6,7,15,23),0,24) == false
range(List(-50,14,2,18,15,23),-51,24) == true
range(List(1,2,3),3,1) == true
range(List(),-2,5) == false