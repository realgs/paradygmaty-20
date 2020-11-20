sealed trait LList[+A]
case object LNil extends LList[Nothing]
case class LElement[+A](elem: A, llist: () => LList[A])

// zad5 (5pkt)
def ldzialanie(llist1:LazyList[Int],llist2:LazyList[Int],operator:(Int, Int)=>Int):LazyList[Int]={
  (llist1,llist2) match {
    case (LazyList(), LazyList()) => LazyList()
    case (_, LazyList()) => llist1
    case (LazyList(), _) => llist2
    case (h1#::t1,h2#::t2) => operator(h1,h2)#::ldzialanie(t1,t2,operator)
  }
}
// operatory
def +(a: Int, b: Int):Int={
  a+b
}
def -(a: Int, b: Int):Int={
  a-b
}
def *(a: Int, b: Int):Int={
  a*b
}
def /(a: Int, b: Int):Int={
  a/b
}
//test
ldzialanie(LazyList(1, 2, 3), LazyList(4, 5, 6), +).toList
ldzialanie(LazyList(1, 2, 3), LazyList(4, 5, 6), -).toList
ldzialanie(LazyList(1, 2, 3), LazyList(4, 5, 6), *).toList
ldzialanie(LazyList(1, 2, 3), LazyList(4, 5, 6), /).toList
ldzialanie(LazyList(1, 2), LazyList(4, 5, 6), +).toList
ldzialanie(LazyList(1, 2, 3), LazyList(4, 5), -).toList
ldzialanie(LazyList(1, 2, 3), LazyList(), *).toList
ldzialanie(LazyList(), LazyList(4, 5, 6), /).toList
ldzialanie(LazyList(), LazyList(), +).toList