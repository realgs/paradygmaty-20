import jdk.javadoc.internal.doclets.formats.html.markup.HtmlTree.A

import scala.::
import scala.annotation.tailrec

//Lista 4 Maciej Kopiński

//drzewo
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

//funkcja liczaca glebokosc drzewa
def depth(tree: BT[Int]): Int={
  def inner(n: Int, t: BT[Int]): Int={
    t match {
      case Empty => n
      case Node(e, l, r) => {
        if(inner(n + 1, l)>inner(n + 1, r)) inner(n + 1, l)
        else inner(n + 1, r)
      }
    }
  }
  inner(0, tree)
}

//lista
sealed trait LList[+A]
case object LNil extends LList[Nothing]
case class LElement[+A](elem: A, llist: () => LList[A])

//Zadanie 1 (3 pkt)

def createTree(n: Int, start: Int, end: Int): BT[Int]={
  def inner(a: Int): BT[Int]={
    val r = scala.util.Random
    if(a==0){
      Empty
    }else{
      Node(r.nextInt(end-start+1)+start, inner(a-1), inner(a-1))
    }
  }
  inner(n)
}

createTree(5, 3, 8)
depth(createTree(5, 3, 8))

//Zadanie 2 (3 pkt)

def mergeTrees(first: BT[Int], second: BT[Int]): BT[Int]={
  if(depth(first)!=depth(second)){
    throw new Exception("Rozne glebokosci drzew!")
  }else{
    def inner(f: BT[Int], s: BT[Int]): BT[Int]={
      (f, s) match {
        case (Empty, Empty) => Empty
        case (Node(v1, l1, r1), Node(v2, l2, r2)) => Node(v1-v2, inner(l1, l2), inner(r1, r2))
      }
    }
    inner(first, second)
  }
}

val tt1=createTree(5, 3, 8)
val tt2=createTree(5, 1, 6)



mergeTrees(tt1, tt2)

//Zadanie 3

//BFS (3 pkt)

def breadthBT(bt1 : BT[Int], bt2 : BT[Int]) : (List[BT[Int]], List[BT[Int]])={
  def inner(queue : List[(BT[Int], BT[Int])]) : (List[BT[Int]], List[BT[Int]])={
    queue match {
      case Nil => (Nil, Nil)
      case (Empty, Empty)::t => ((Empty)::inner(t)._1, (Empty)::inner(t)._2)
      case (Node(v1, l1, r1), Node(v2, l2, r2))::t=> (Node(v1, l1, r1)::inner(t)._1, Node(v2, l2, r2)::inner(t)._2)
    }
  }
  inner(List((bt1, bt2)))
}


def checkLists(l1: List[BT[Int]], l2: List[BT[Int]]): (List[BT[Int]], List[BT[Int]])={
  val a1=l1.toArray
  val a2=l2.toArray
  var i=a1.length-1
  var j=a2.length-1
  def inner(arr1: Array[BT[Int]], index: Int, value: BT[Int]): List[BT[Int]]={
    var i=index
    while(i>=0){
      arr1(i) match {
        case Empty => ()
        case Node(v, l, r) => if(arr1(i)==value && value != Empty) arr1(i)=Node(-1, l, r) else if(arr1(i)==value && value == Empty) arr1(i)=Empty
      }
      i-=1
      i/=2
    }
    arr1.toList
  }
  (inner(a1, i, a1(i)), inner(a2, j, a2(j)))
}

def createTreeFromList(list: List[BT[Int]]): BT[Int]={
  def inner(index: Int, list: List[BT[Int]]): BT[Int]={
    val i=index
    val arr=list.toArray
    val node=arr(0)
      arr(index) match {
        case Empty => Empty
        case Node(v, l, r) => Node(v, if(2*i+1>arr.length) Empty else inner(2*i+1, list), if(2*i+2>arr.length) Empty else inner(2*i+2, list))
      }
  }
  inner(0, list)
}

def deleteDuplicatesBFS(first: BT[Int], second: BT[Int]): (BT[Int], BT[Int])={
  if(depth(first)!=depth(second)){
    throw new Exception("Rozne glebokosci drzew!")
  }else{
    (createTreeFromList(checkLists(breadthBT(first, second)._1, breadthBT(first, second)._2)._1), createTreeFromList(checkLists(breadthBT(first, second)._1, breadthBT(first, second)._2)._2))
  }
}


//DFS (1 pkt)

def deleteDuplicatesDFS(first: BT[Int], second: BT[Int]): (BT[Int], BT[Int])={
  if(depth(first)!=depth(second)){
    throw new Exception("Rozne glebokosci drzew!")
  }else{
    def inner(bt1 : BT[Int], bt2 : BT[Int]) : (BT[Int], BT[Int])={
      (bt1, bt2) match {
        case (Empty, Empty) => (Empty, Empty)
        case (Node(v1, Empty, Empty), Node(v2, Empty, Empty)) => if(v1==v2) (Empty, Empty) else (Node(v1, Empty, Empty), Node(v2, Empty, Empty))
        case (Node(v1, l1, r1), Node(v2, l2, r2)) => {
          val (lTree1, lTree2)=inner(l1, l2)
          val (rTree1, rTree2)=inner(r1, r2)
          (lTree1, lTree2, rTree1, rTree2) match {
            case (Empty, Empty, Empty, Empty) =>if(v1==v2) (Empty, Empty) else (Node(v1, Empty, Empty), Node(v2, Empty, Empty))
            case (l1 ,l2 ,r1, r2) => if(v1==v2) (Node(-1, l1, r1), Node(-1, l2, r2)) else (Node(v1, l1, r1), Node(v2, l2, r2))
          }
        }
      }
    }
    inner(first, second)
  }
}

val t1 = Node(1,
  Node(2,
    Node(4,
      Empty,
      Empty
    ),
    Empty
  ),
  Node(3,
    Node(5,
      Empty,
      Node(6,
        Empty,
        Empty
      )
    ),
    Empty
  )
)

val t2 = Node(1,
  Node(2,
    Node(4,
      Empty,
      Empty
    ),
    Empty
  ),
  Node(3,
    Node(5,
      Empty,
      Node(7,
        Empty,
        Empty
      )
    ),
    Empty
  )
)

deleteDuplicatesBFS(t1, t2);

deleteDuplicatesDFS(t1, t2);

//Zadanie 4 (5 pkt)

def eachNElement[A](llist: LazyList[A], n: Int, m: Int): LazyList[A]={
  def inner(position: Int, counter: Int, l: LazyList[A]): LazyList[A]={
    l match {
      case LazyList() => LazyList()
      case h#::t => if(position==m && counter==n) h#::LazyList()
      else if(position==m && counter!=n) LazyList()
      else if(counter==n) h#::inner(position+1, 1, t)
      else inner(position+1, counter+1, t)
    }
  }
  llist.head#::inner(1, 1, llist.tail)
}

eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 8).toList
eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3).toList
eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4).toList

//Zadanie 5 (5 pkt)

def ldzialanie(list1: LazyList[Double], list2: LazyList[Double], function: (Double, Double) => Double): LazyList[Double]={
  (list1, list2) match {
    case (LazyList(), LazyList()) => LazyList()
    case (LazyList(), _) => list2
    case (_, LazyList()) => list1
    case (h1#::t1, h2#::t2) => function(h1, h2)#::ldzialanie(t1, t2, function)
  }
}

def add(a: Double, b: Double): Double={
  a+b
}

def subtract(a: Double, b: Double): Double={
  a-b
}

def multiply(a: Double, b: Double): Double={
  a*b
}

def divide(a: Double, b: Double): Double={
  a/b
}

ldzialanie(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), add).toList
ldzialanie(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), subtract).toList
ldzialanie(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), multiply).toList
ldzialanie(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), divide).toList