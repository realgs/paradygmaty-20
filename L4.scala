sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A,left:BT[A],right:BT[A])extends BT[A]

sealed trait nlist[+A]
case class Koniec[+A]() extends nlist[A]
case class Element[+A](elem: A,list: nlist[A]) extends nlist[A]

sealed  trait llist[+A]
case class LKoniec[+A]() extends llist[A]
case class LElement[+A](elem: A,tail: ()=>llist[A]) extends llist[A]

class L4 {

  //zadanie 1 (3 pkt)
  def generateTree(n: Int, down: Int, up: Int): BT[Int] = {
    val r = scala.util.Random

    def helper(n: Int): BT[Int] =
      if (n == 0) Node(down + r.nextInt(up - down), Empty, Empty)
      else Node(down + r.nextInt(up - down), helper(n - 1), helper(n - 1))

    if (down < 0 || up < 0 || down > up || n <= 0) Empty
    else helper(n - 1)
  }

  //zadanie 2 (3 pkt)
  // Złożoność obliczeniowa O(n), gdzie n - ilość węzłów w drzewie
  def subOfTrees(tree1: BT[Int], tree2: BT[Int]): BT[Int] =
    (tree1, tree2) match {
      case (Node(v1, l1, r1), Node(v2, l2, r2)) => Node(v1 - v2, subOfTrees(l1, l2), subOfTrees(r1, r2))
      case (Empty, Empty) => Empty
      case (_, _) => throw new Exception("Incorrect Tree")
    }

  //zadanie 3 (4 pkt)
  //wgłąb
  // Złożoność obliczeniowa O(n), gdzie n - ilość węzłów w drzewie
  def sameElemDelDepth(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) =
    (tree1, tree2) match {
      case (Node(v1, l1, r1), Node(v2, l2, r2)) =>
        val (lt1, lt2) = sameElemDelDepth(l1, l2)
        val (rt1, rt2) = sameElemDelDepth(r1, r2)
        if (v1 == v2) {
          if (lt1 == Empty && rt1 == Empty) (Empty, Empty)
          else (Node(-1, lt1, rt1), Node(-1, lt2, rt2))
        }
        else (Node(v1, lt1, rt1), Node(v2, lt2, rt2))
      case (Empty, Empty) => (Empty, Empty)
      case (_, _) => throw new Exception("Incorrect Tree")
    }

  //wszerz
  // Gdy drzewa róźnią się całkowicie (pesymistyczna) O(n^2) (n-ilość węzłów w drzewie)
  // Gdy drzewa są takie same (optymistyczna) O(n) (n-ilość węzłów w drzewie)
  def checkSubTree(queue:List[(BT[Int],BT[Int])]):Boolean =
    queue match{
      case Nil => true
      case (Empty,Empty)::tail => checkSubTree(tail)
      case (Node(v1,l1,r1),Node(v2,l2,r2))::tail =>
        if(v1==v2) checkSubTree(tail:::List((l1,l2),(r1,r2)))
        else false
      case(_,_)::_ => throw new Exception("Incorrect Tree")
    }

  def sameElemDelBreadth(tree1:BT[Int],tree2:BT[Int]):(BT[Int],BT[Int]) ={
    def delete(t1:BT[Int],t2:BT[Int]):(BT[Int],BT[Int]) ={
      val Node(v1,l1,r1) = t1
      val Node(v2,l2,r2) = t2
      val (x,y) = if(v1==v2) (-1,-1) else (v1,v2)

      (checkSubTree(List((l1,l2))),checkSubTree(List((r1,r2))))match{
        case (true,true) =>
          if(x == -1 && y == -1) (Empty,Empty)
          else (Node(x,Empty,Empty),Node(y,Empty,Empty))
        case (true,false) =>
          val (right1,right2) = delete(r1,r2)
          (Node(x,Empty,right1),Node(y,Empty,right2))
        case (false,true) =>
          val (left1,left2) = delete(l1,l2)
          (Node(x,left1,Empty),Node(y,left2,Empty))
        case (false,false) =>
          val (left1,left2) = delete(l1,l2)
          val (right1,right2) = delete(r1,r2)
          (Node(x,left1,right1),Node(y,left2,right2))
      }
    }
    (tree1,tree2) match{
      case (Empty,Empty) => (Empty,Empty)
      case (Node(_,_,_),Node(_,_,_)) => delete(tree1,tree2)
      case (_,_) => throw new Exception("Incorrect Tree")
    }
  }

  //zadanie 4 (5 pkt)
  def eachNElement(lazyList: llist[Int], n: Int, m: Int): llist[Int] = {
    def iteration(list: llist[Int], i: Int, j: Int): llist[Int] = {
      if (i * n + j < m) {
        if (j > 0)
          list match {
            case LElement(_, tail) =>
              iteration(tail(), if (j + 1 == n) i + 1 else i, if (j + 1 == n) 0 else j + 1)
            case LKoniec() => LKoniec()
          }
        else
          list match {
            case LElement(elem, tail) => LElement(elem, () =>
              iteration(tail(), if (j + 1 == n) i + 1 else i, if (j + 1 == n) 0 else j + 1))
            case LKoniec() => LKoniec()
          }
      }
      else LKoniec()
    }

    if (n <= 0 || m < 0) throw new Exception("Wrong parameters")
    else iteration(lazyList, 0, 0)
  }

  //zadanie 5 (5 pkt)
  def ldzialanie(lazyList1: llist[Int], lazyList2: llist[Int], sign: Char): llist[Int] = {
    def operation(lazyList1: llist[Int], lazyList2: llist[Int],fun:Int=>Int=>Int):llist[Int] =
      (lazyList1,lazyList2) match {
        case (LKoniec(),_) => lazyList2
        case (_,LKoniec()) => lazyList1
        case (LElement(h1,t1),LElement(h2,t2)) => LElement(fun(h1)(h2),()=>operation(t1(),t2(),fun))
      }
    sign match {
      case '+' => operation(lazyList1,lazyList2,(x:Int)=>(y:Int)=>x+y)
      case '-' => operation(lazyList1,lazyList2,(x:Int)=>(y:Int)=>x-y)
      case '*' => operation(lazyList1,lazyList2,(x:Int)=>(y:Int)=>x*y)
      case '/' => operation(lazyList1,lazyList2,(x:Int)=>(y:Int)=>x/y)
      case _ => throw new Exception("Wrong sign")
    }
  }
}
