//BinaryTree
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]
//zad 1(3pkt)
val rand = scala.util.Random
def generateTree(n:Int,a:Int,b:Int):BT[Int]= {
  n match {
    case 0 => Empty
    case _ => Node(rand.nextInt((b-a)+1),generateTree(n-1,a,b),generateTree(n-1,a,b))
  }
}
//test
generateTree(0,1,2)
generateTree(1,1,4)
generateTree(2,1,10)
generateTree(5,1,10)
//zad 2(3pkt)
def subTree(tree1: BT[Int], tree2: BT[Int]): BT[Int] = {
  (tree1, tree2) match {
    case (Empty, Empty) => Empty
    case (Node(e1, l1, r1), Node(e2, l2, r2)) => Node(e1 - e2, subTree(l1, l2), subTree(r1, r2))
    case (_,_) => Empty
  }
}
//test
subTree(generateTree(3,2,10),generateTree(3,2,10))
subTree(generateTree(0,2,10),generateTree(0,2,10))
subTree(generateTree(1,2,10),generateTree(3,2,10))
subTree(generateTree(3,2,10),generateTree(1,2,10))

//lazylist
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