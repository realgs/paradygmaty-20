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
  println(difTree(tree1,tree2) + "\n");
  //println(difTree(tree1, tree3) + "/n");//throw Exception "Różna głębokość drzew"

  //Zadanie 4 Punkty: 5

  val lazyList1:LazyList[Int] = LazyList[Int](5,6,3,2);
  val lazyList2:LazyList[Int] = LazyList[Int](1,2,3,4,5,6,7,8,9,10,11,12,13,14,15);
  val llistTest:LazyList[Int] = LazyList[Int](5,6,3,2,1,12,10,8,6);
  val emptyLlist:LazyList[Int] = LazyList[Int]();

  def eachNElement(lazyList: LazyList[Int], every:Int, toLast:Int):LazyList[Int]= {
    def thisNumber(llist:LazyList[Int], n:Int, m:Int,result:LazyList[Int]):LazyList[Int] ={
      llist match {
        case LazyList() => LazyList();
        case head#::tail =>
          if(n == 0){
            head#::thisNumber(tail, n + every, m + 1, result);//correct(n + every)
          }else if(m == 0){
            result.head#::thisNumber(tail,n-1,m+1,result);
          } else if(m <= toLast){
             if(m == toLast && n == 1){
              thisNumber(LazyList(), - 1, m + 2, result);
            }else{
               thisNumber(tail,n - 1,m + 1,result);
             }
        }  else {
            thisNumber(LazyList(), - 1,m + 2,result);
          }
      }
    }
    if(every <= 0 || toLast <= 0){
      LazyList();
    }else{
      thisNumber(lazyList,every,0,lazyList.head#::LazyList());
    }
  }

  //lazyList.head#::result;
  //println(llistTest.take(8).toList);

  println("Zadanie 4")
  println("LazyList = " + eachNElement(llistTest,1,3).toList);
  println("LazyList = " + eachNElement(lazyList1,2,4).toList);
  println("LazyList = " + eachNElement(lazyList2,0,5).toList);
  println("LazyList = " + eachNElement(lazyList2,0,0).toList);
  println("EmptyLazyList = " + eachNElement(emptyLlist,0,2).toList);
  println("LazyList2 = " + eachNElement(lazyList2,1,10).toList + "\n");

  //Zadanie 5 punkty: 5
  val llxs:LazyList[Int] = LazyList[Int](4, -6, 14)
  val lxs:LazyList[Int] = LazyList[Int](3,4,-7);
  val emptyll:LazyList[Int] = LazyList[Int]();

  val + = (value1: Int) => (value2:Int) => value1 + value2;
  val * = (value1: Int) => (value2:Int) => value1 * value2;
  val - = (value1: Int) => (value2:Int) => value1 - value2;
  val / = (value1: Int) => (value2:Int) => value1 / value2;

  def lazyFunc[Int](lazyList1: LazyList[Int],lazyList2: LazyList[Int])(param:Int=>Int=>Int): LazyList[Int] = {
    (lazyList1, lazyList2) match {
      case (LazyList(), LazyList()) => LazyList()
      case (_,LazyList()) => LazyList();
      case (LazyList(),_) => LazyList();
      case(head1#::tail1,head2#::tail2) =>  param(head1)(head2)#::lazyFunc(tail1,tail2)(param);
    }
  }

  println("Zadania 5")
  println((+)(5)(7));
  println("lista1 = " + llxs.toList);
  println("lista2 = " + lxs.toList);
  println("+ = " + lazyFunc(llxs,lxs)(+).toList);
  println("* = " + lazyFunc(llxs,lxs)(*).toList);
  println("- = " + lazyFunc(llxs,lxs)(-).toList);
  println("* = " + lazyFunc(llxs,lxs)(/).toList);
  println(lazyFunc(llxs,emptyll)(+).toList);
  println(lazyFunc(emptyll,lxs)(*).toList);
  println(lazyFunc(emptyll,emptyll)(/).toList);
}
