object lab3 {
  def split(inputList:List[Int]):List[List[Int]] =
  {
    def minus(currentList:List[Int], list:List[Int]): List[Int] =
    {
      if (currentList == Nil) list
      else if (currentList.head < 0) minus(currentList.tail, list:+currentList.head)
      else minus(currentList.tail, list)
    }
    def minusOdd(currentList:List[Int], list:List[Int]) : List[Int] =
    {
      if (currentList == Nil) list
      else if (currentList.head%2 == -1) minusOdd(currentList.tail, list:+currentList.head)
      else minusOdd(currentList.tail, list)
    }
      List(minus(inputList, Nil), minusOdd(inputList, Nil))
    }
  
  split(List(-3,-4, -1)) == List(List(-3, -4, -1), List(-3, -1))
  split(Nil) == List(Nil, Nil)
  split(List(3,1,4,5)) == List(Nil, Nil)
  split(List(-1, -1, -3, -5, 4, 4, 0, -3, -4)) == List(List(-1, -1, -3, -5, -3, -4), List(-1,-1,-3,-5,-3))

// złożoność obliczeniowa O(n), złożoność pamięciowa O(n)
  def length[A](list:List[A]):Int =
    if(list == Nil) 0
    else 1 + length(list.tail)
    
  length(List(1,3,4,5)) == 4
  length(Nil) == 0
  length(List("this", "is", "how", "we", "do", "it")) == 5
 
 // złożoność obliczeniowa O(n+m), złożoność pamięciowa O(duża)
 def merge[A](firstList:List[A], secondList:List[A]): List[A] =
    if (firstList == Nil)
      if(secondList != Nil)
        secondList.head::merge(secondList.tail, firstList)
      else
        Nil
    else
      firstList.head::merge(secondList, firstList.tail)
     
   
   merge(List(1,3,5,7), List(2,4,6,8)) == List(1,2,3,4,5,6,7,8)
   merge(List(1,3), List(2,4,5,6,7)) == List(1,2,3,4,5,6,7)
   merge(List(1,3,5,6,7), List(2,4)) == List(1,2,3,4,5,6,7)

def mergeThree[A](firstList:List[A], secondList:List[A], thirdList:List[A]) : List[A] =
    if (thirdList == Nil && secondList == Nil) firstList
    else
      if (thirdList == Nil)
        if(secondList == Nil) Nil
        else mergeThree(firstList:+secondList.head, secondList.tail, thirdList)
      else mergeThree(firstList:+thirdList.head, secondList, thirdList.tail)
      
mergeThree(List(1), List(3), List(2)) == List(1,2,3)
mergeThree(List(1), List(4), List(2,3)) == List(1,2,3,4)
mergeThree(List(1,2,3), List(7,8), List(4,5,6)) == List(1,2,3,4,5,6,7,8)
mergeThree(List(1,2,3), List(4,5), Nil) == List(1,2,3,4,5)
mergeThree(Nil, Nil, List(1,2)) == List(1,2)
mergeThree(Nil, List(3,4), List(1,2)) == List(1,2,3,4)
mergeThree(Nil, Nil, Nil) == Nil
}
