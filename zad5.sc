def ldzialanie(list1:LazyList[Int],list2:LazyList[Int],operator:String):LazyList[Int] = {
  operator match {
    case "+" => def add(list1: LazyList[Int], list2: LazyList[Int]): LazyList[Int] =
      (list1, list2) match {
        case (LazyList(), LazyList()) => LazyList()
        case (LazyList(), list2) => list2
        case (list1, LazyList()) => list1
        case (h1 #:: t1, h2 #:: t2) => (h1 + h2) #:: add(t1, t2)
      }
      add(list1,list2)
    case "-" => def substract(list1: LazyList[Int], list2: LazyList[Int]): LazyList[Int] =
      (list1, list2) match {
        case (LazyList(), LazyList()) => LazyList()
        case (LazyList(), list2) => list2
        case (list1, LazyList()) => list1
        case (h1 #:: t1, h2 #:: t2) => (h1 - h2) #:: substract(t1, t2)
      }
      substract(list1,list2)
    case "*" => def multiply(list1: LazyList[Int], list2: LazyList[Int]): LazyList[Int] =
      (list1, list2) match {
        case (LazyList(), LazyList()) => LazyList()
        case (LazyList(), list2) => list2
        case (list1, LazyList()) => list1
        case (h1 #:: t1, h2 #:: t2) => (h1 * h2) #:: multiply(t1, t2)
      }
      multiply(list1,list2)
    case "/" => def divide(list1: LazyList[Int], list2: LazyList[Int]): LazyList[Int] =
      (list1, list2) match {
        case (LazyList(), LazyList()) => LazyList()
        case (LazyList(), list2) => list2
        case (list1, LazyList()) => list1
        case (h1 #:: t1, h2 #:: t2) => if (h2 != 0) (h1 / h2) #:: divide(t1, t2) else throw new Exception("Tried to divide by 0")
      }
      divide(list1,list2)
    case _ => throw new Exception("Unsupported Operation Exception")
  }
}

ldzialanie(LazyList(1,2,3,4),LazyList(1,2,3,4),"+").take(4).force
ldzialanie(LazyList(1,2,3,4),LazyList(1,2,3,4),"-").take(4).force
ldzialanie(LazyList(1,2,3,4),LazyList(1,2,3,4),"*").take(4).force
ldzialanie(LazyList(1,2,3,4),LazyList(1,2,3,4),"/").take(4).force
//ldzialanie(LazyList(1,2,3,4),LazyList(1,2,3,4),"^")

val lfib= {
  def lfibHelp(x: Int, y: Int): LazyList[Int] =
    x #:: lfibHelp(y,x+y)
  lfibHelp(0,1)
}

lfib.take(5).force
ldzialanie(lfib,lfib,"+").take(5).force
ldzialanie(lfib,lfib,"-").take(5).force
ldzialanie(lfib,lfib,"*").take(5).force
ldzialanie(lfib,lfib,"/").take(5).force
//ldzialanie(lfib,lfib,"^").take(5).force