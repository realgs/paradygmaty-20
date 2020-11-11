import scala.annotation.tailrec
import scala.util.Random

object Lista4 extends App {

  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

  val maxNodeVal = 5


  // zad 1 3pkt
  def generujDrzewo[A](levels:Int):BT[Int]=
    if(levels<=0)Node(Random.nextInt(maxNodeVal)+1,Empty,Empty)
    else Node(Random.nextInt(maxNodeVal)+1,generujDrzewo(levels -1),generujDrzewo(levels -1))

  println(generujDrzewo(2))

  // zad 2 3pkt
  def odejmijDrzewo[A](tree1:BT[Int],tree2:BT[Int]):BT[Int]=
    (tree1,tree2)match {
      case (Node(val1,Empty,Empty),Node(val2,Empty,Empty)) => Node(val1-val2,Empty,Empty)
      case (Node(_,_,Empty),Node(_,_,_)) => throw new Exception("Drzewo 1 mniejsze od drzewa 2")
      case (Node(_,Empty,_),Node(_,_,_)) => throw new Exception("Drzewo 1 mniejsze od drzewa 2")
      case (Node(_,_,_),Node(_,_,Empty)) => throw new Exception("Drzewo 2 mniejsze od drzewa 1")
      case (Node(_,_,_),Node(_,Empty,_)) => throw new Exception("Drzewo 2 mniejsze od drzewa 1")
      case (Node(val1,left1,right1),Node(val2,left2,right2)) => Node(val1-val2,odejmijDrzewo(left1,left2),odejmijDrzewo(right1,right2))
    }


  val tree1 = generujDrzewo(1)
  val tree2 = generujDrzewo(1)
  println(tree1)
  println(tree2)
  println(odejmijDrzewo(tree1, tree2))

  val tree3 = generujDrzewo(1)
  val tree4 = generujDrzewo(2)
  println(tree3)
  println(tree4)
  //println(odejmijDrzewo(tree3, tree4))

}
