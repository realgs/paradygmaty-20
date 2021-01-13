//Zadanie 4 (5pkt)
def eachNElement(list: LazyList[Int], n: Int, m: Int): LazyList[Int] = {
  def helper(list: LazyList[Int], index: Int): LazyList[Int] = {
    if (list == LazyList()) list
    else {
      if (index >= m) LazyList()
      else {
        if (index % n == 0) list.head #:: helper(list.tail, index + 1)
        else helper(list.tail, index + 1)
      }
    }
  }
  if (list.size < n) throw new IllegalArgumentException("Index n musi być mniejszy od rozmiaru LazyList")
  if (list.size < m) throw new IllegalArgumentException("Index m musi być mniejszy od rozmiaru LazyList")
  if (m <= 0) throw new IllegalArgumentException("m jest mniejszy od 1")
  if (n <= 0) throw new IllegalArgumentException("n jest mniejszy od 1")
  helper(list, 0)
}

eachNElement(LazyList(5,6,3,2,1),2,3).force
eachNElement(LazyList(5,6,3,2,1),2,4).force
eachNElement(LazyList(1,2,3,4,5,6,7,8,9),1,5).force
eachNElement(LazyList(6,5,4,3,2,1),5,1).force

//Zadanie 5 (5pkt)
def ldzialanie[A](l1:LazyList[A], l2:LazyList[A], operator:(A,A)=>A):LazyList[A]={
  (l1,l2) match {
    case (LazyList(),LazyList()) => LazyList()
    case (list1,LazyList()) => list1
    case (LazyList(),list2) => list2
    case (list1,list2)  => operator(list1.head,list2.head)#::ldzialanie(list1.tail,list2.tail,operator)
  }
}

ldzialanie(LazyList(1,2,3),LazyList(2,3,4,5),(x:Int,y:Int) => x + y).toList
ldzialanie(LazyList(1,2,3),LazyList(2,3,4,5),(x:Int,y:Int) => x - y).toList
ldzialanie(LazyList(1,2,3),LazyList(2,3,4,5),(x:Int,y:Int) => x * y).toList
ldzialanie(LazyList(1.0,2.0,3.0),LazyList(2.0,3.0,4.0,5.0),(x:Double,y:Double) => x / y).toList

