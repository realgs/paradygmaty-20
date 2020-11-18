import jdk.javadoc.internal.doclets.formats.html.markup.HtmlTree.A

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
      case Node(e, l, r) => inner(n+1, l)
    }
  }
  inner(0, tree)
}

//lista
sealed trait LList[+A]
case object LNil extends LList[Nothing]
case class LElement[+A](elem: A, llist: () => LList[A])

//Zadanie 1

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

//Zadanie 2

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

val t1=createTree(5, 3, 8)
val t2=createTree(5, 1, 6)

mergeTrees(t1, t2)

//Zadanie 3

def deleteDuplicates(first: BT[Int], sexond: BT[Int]): (BT[Int], BT[Int])={
  (Empty, Empty)
}

//Zadanie 4

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

//Zadanie 5

def ldzialanie(list1: LazyList[Int], list2: LazyList[Int], function: Int): LazyList[Int]={
  LazyList()
}
