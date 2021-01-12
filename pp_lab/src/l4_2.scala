import scala.annotation.tailrec

object l4_2 {
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

  //Funkcja pomocnicza wyswietlajaca drzewa
  def printBT[A](tree: BT[A]): Unit = {
    def innerPrintBT(tree: BT[A], depth: Int): Unit =
      tree match {
        case Node(value, leftNode, rightNode) =>
          innerPrintBT(rightNode, depth+1)
          for (i <- 0 until depth-1) print("...")
          print(value + "\n")
          innerPrintBT(leftNode, depth+1)
        case Empty =>
          for (i <- 0 until depth-1) print("...")
          print("||\n")
      }
    innerPrintBT(tree, 0)
  }

  //ZADANIE 1 (3pkt)
  def createTree(depth: Int, from: Int, to: Int): BT[Int] = {
    val r = scala.util.Random
    depth match {
      case 0 => Empty
      case k => Node(from+r.nextInt(to-from), createTree(k-1, from, to), createTree(k-1, from, to))
    }
  }

  //ZADANIE 2 (3pkt)
  def subtract(tree1: BT[Int], tree2: BT[Int]): BT[Int] =
    (tree1, tree2) match {
      case (Node(value1, leftNode1, rightNode1), Node(value2, leftNode2, rightNode2)) =>
        Node(value1 - value2, subtract(leftNode1, leftNode2), subtract(rightNode1, rightNode2))
      case _ => Empty
    }

  //ZADANIE 3 (4pkt)
  def removeDuplicatesDFS(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) = {
    (tree1, tree2) match {
      case (Empty, Empty) => (Empty, Empty)
      case (Node(value1, leftNode1, rightNode1), Node(value2, leftNode2, rightNode2)) =>
        val (left1, left2) = removeDuplicatesDFS(leftNode1, leftNode2)
        val (right1, right2) = removeDuplicatesDFS(rightNode1, rightNode2)
        if(value1 == value2 && left1 == Empty && right1 == Empty) (Empty, Empty)
        else if(value1 == value2) (Node(-1, left1, right1), Node(-1, left2, right2))
        else (Node(value1, left1, right1), Node(value2, right1, right2))
    }
  }

  def removeDuplicatesBFS(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) = {
    def innerRemoveDuplicatesBFS(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) = {
      val Node(value1, leftNode1, rightNode1) = tree1
      val Node(value2, leftNode2, rightNode2) = tree2
      val (x,y) = if(value1 == value2) (-1,-1) else (value1, value2)
      (areSubtreesEqual(List((leftNode1, leftNode2))), areSubtreesEqual(List((rightNode1, rightNode2)))) match {
        case (false, false) =>
          val (left1, left2) = innerRemoveDuplicatesBFS(leftNode1, leftNode2)
          val (right1, right2) = innerRemoveDuplicatesBFS(rightNode1, rightNode2)
          (Node(x, left1, right1), Node(y, left2, right2))
        case (true, false) =>
          val (right1, right2) = innerRemoveDuplicatesBFS(rightNode1, rightNode2)
          (Node(x, Empty, right1), Node(y, Empty, right2))
        case (false, true) =>
          val (left1, left2) = innerRemoveDuplicatesBFS(leftNode1, leftNode2)
          (Node(x, left1, Empty), Node(y, left2, Empty))
        case (true, true) =>
          if(x == -1 && y == -1) (Empty,Empty)
          else (Node(x, Empty, Empty), Node(y, Empty, Empty))
      }
    }
    (tree1, tree2) match {
      case (Empty, Empty) => (Empty, Empty)
      case (Node(_, _, _), Node(_, _, _)) => innerRemoveDuplicatesBFS(tree1, tree2)
    }
  }

  @tailrec
  def areSubtreesEqual(xs: List[(BT[Int], BT[Int])]): Boolean =
    xs match {
      case Nil => true
      case (Empty, Empty) :: tail => areSubtreesEqual(tail)
      case (Node(value1, leftNode1, rightNode1), Node(value2, leftNode2, rightNode2)) :: tail =>
        if(value1 == value2) areSubtreesEqual(tail ::: List((leftNode1, leftNode2), (rightNode1, rightNode2)))
        else false
    }

  //ZADANIE 4 (5 pkt)
  def eachNElement[A](xs: LazyList[A], n: Int, howManyElements: Int): LazyList[A] = {
    if (n < 1 || howManyElements < 0) throw new Exception("Invalid arguments")
    def helper(xs: LazyList[A], elCounter: Int): LazyList[A] = {
      xs match {
        case h #:: t => if (elCounter == howManyElements) LazyList()
        else if (elCounter % n == 0) h #:: helper(t, elCounter+1)
        else helper(t, elCounter+1)
        case _ => LazyList()
      }
    }
    helper(xs, 0)
  }

  //ZADANIE 5 (5pkt)
  //Gdy wejściowe listy mają różną liczbę elementów: przy + oraz - brakujący element uznaje się za 0
  //Przy * brakujący element uznaje się za 1
  //Przy dzieleniu: x/Nil = x oraz Nil/x = 0
  def ldzialanie2(xs1: LazyList[Double], xs2: LazyList[Double], operator: Char): LazyList[Double] =
    (operator, xs1, xs2) match {
      case ('+'|'-'|'*'|'/', LazyList(), LazyList()) => LazyList()
      case ('+'|'-'|'*'|'/', list, LazyList()) => list
      case ('+'|'*', LazyList(), list) => list
      case ('-', LazyList(), h #:: t) => -h #:: ldzialanie2(LazyList(), t, '-')
      case ('/', LazyList(), _ #:: t) => 0 #:: ldzialanie2(LazyList(), t, '/')
      case ('+', h1 #:: t1, h2 #:: t2) => (h1 + h2) #:: ldzialanie2(t1, t2, '+')
      case ('-', h1 #:: t1, h2 #:: t2) => (h1 - h2) #:: ldzialanie2(t1, t2, '-')
      case ('*', h1 #:: t1, h2 #:: t2) => (h1 * h2) #:: ldzialanie2(t1, t2, '*')
      case ('/', h1 #:: t1, h2 #:: t2) => (h1 / h2) #:: ldzialanie2(t1, t2, '/')
      case _ => throw new Exception("Undefined arithmetic operation")
    }

  //Wersja z funkcja zamiast chara jako operator
  def ldzialanie(xs1: LazyList[Double], xs2: LazyList[Double], operator: (Double, Double) => Double): LazyList[Double] =
    (xs1, xs2) match {
      case (LazyList(), _) => xs2
      case (_, LazyList()) => xs1
      case (h1 #:: t1, h2 #:: t2) => operator(h1, h2) #:: ldzialanie(t1, t2, operator)
    }


  val + : (Double, Double) => Double = (a, b) => a + b
  val - : (Double, Double) => Double = (a, b) => a - b
  val * : (Double, Double) => Double = (a, b) => a * b
  val / : (Double, Double) => Double = (a, b) => a / b
}

