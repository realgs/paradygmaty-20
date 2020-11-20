import scala.util.Random;

object Drzewa extends App{

  val r = scala.util.Random;
  println(r.nextInt(10));

  sealed trait Tree[+A];
  //case class Node[+A](val value: Int) extends Tree[A];
  //val name = Node(5);

  case object Empty extends Tree[Nothing];
  case class Node[+A](value:A,left:Tree[A],right:Tree[A])extends Tree[A];

  //Zadanie 1 Punkty: 3
  def genTree(depth: Int, range:Int): Tree[Int] = {
    if(depth >= 0){
      Node(Random.nextInt(range),genTree(depth - 1,range),genTree(depth - 1,range));
    }else return Node(0, Empty, Empty);
  }

  println("Zadanie 1")
  println(genTree(1, 10));
  println(genTree(0, 5));
  println(genTree(-2, 10));

  //Zadanie 2 Punkty: 3
  def lengthTree(tree:Tree[Int]): Int ={

    def eRec(tree: Tree[Int], acc: Int): Int = {
      tree match {
        case Empty         => acc;
        case Node(v, l, r) => eRec( l, acc + 1) max eRec( r, acc + 1);
      }
    }
    eRec(tree, 0);
  }

  //trees for tests
  val tree1 = genTree(2,10);
  val tree2 = genTree(2, 10);
  val tree3 = genTree(0, 10);

  def difTree(tree1: Tree[Int],tree2: Tree[Int]): Tree[Int] ={
   if(lengthTree(tree1)==lengthTree(tree2)){
     (tree1, tree2)match{
       case (Node(v1,Empty, Empty),Node(v2,Empty, Empty)) => Node(v1-v2,Empty,Empty);
       case (Node(v1,l1,r1),Node(v2,l2,r2)) => Node(v1 - v2,difTree(l1,l2),difTree(r1,r2));
      }
   }else{
      throw new Exception("Różna głębokość drzew");
   }
  }

  println();
  println("Zadanie 2");
  println(difTree(tree1,tree2));
  println(difTree(tree1, tree3));
}