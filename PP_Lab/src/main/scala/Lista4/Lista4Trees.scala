package Lista4

object Lista4Trees extends App{
  // Tree
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]


  // Exceptions
  class WrongValueOfArguments extends Exception


  // Test Methods
  // used for testing exc 1
  def isTreeFull(tree: BT[Int]): Boolean = {
    tree match {
      case Empty => true
      case Node(_, l, r) =>
        def innerIsTreeFull(leftTree: BT[Int], rightTree: BT[Int]): Boolean ={
          (leftTree, rightTree) match {
            case(Empty, Empty) => true
            case(Empty, Node(_, _, _)) => false
            case(Node(_, _, _), Empty) => false
            case(Node(_, l1, r1), Node(_, l2, r2)) => innerIsTreeFull(l1, r1) && innerIsTreeFull(l2, r2)
          }
      }
        innerIsTreeFull(l, r)
    }
  }

  // used for testing exc 1
  def isTreeDepthRight(tree: BT[Int], expectedLevel: Int): Boolean = {
    tree match {
      case Empty => false
      case Node(_, l, r) =>
        def innerIsTreeDepthRight(leftTree: BT[Int], rightTree: BT[Int], level: Int): Boolean ={
          (leftTree, rightTree) match {
            case(Empty, Empty) => expectedLevel == 0
            case(Node(_, l1, r1), Node(_, l2, r2)) => innerIsTreeDepthRight(l1, r1, level-1) == innerIsTreeDepthRight(l2, r2, level-1)
            case(_, _) => false
          }
        }
        innerIsTreeDepthRight(l, r, expectedLevel)
    }
  }

  // tylko na uzytek wyswietlania i sprawdzania czy wszystko jest okej
  def inorder[A](bt:BT[A]):List[A] = bt match {
    case Node(v,l,r) => inorder(l) ::: v :: inorder(r)
    case Empty => Nil
  }


  // zadanie 1 (3pkt)
  // dla amountOfLevels == 0 zwraca drzewo z samym korzeniem z wartoscia - potomkami korzenia sa Empty
  // dla amountOfLevels < 0 rzuca blad
  def createTree(amountOfLevels: Int, leftInterval: Int, rightInterval: Int): BT[Int] = {
    if (amountOfLevels < 0 || rightInterval < leftInterval) throw new WrongValueOfArguments
    val randomInt = scala.util.Random
    def innerCreateTree(levels: Int): BT[Int] = {
      if(levels == (-1)) Empty
      else Node(randomInt.nextInt(rightInterval-leftInterval+1)+leftInterval, innerCreateTree(levels-1), innerCreateTree(levels-1))
    }
    innerCreateTree(amountOfLevels)
  }


  // zadanie 2 (3pkt)
  def subtractTrees(leftTree: BT[Int], rightTree: BT[Int]):BT[Int] = {
    (leftTree, rightTree) match{
      case (Node(value1, left1, right1), Node(value2, left2, right2)) => Node(value1 - value2, subtractTrees(left1, left2), subtractTrees(right1, right2))
      case (_, _) => Empty
    }
  }

  val tree1 = createTree(3, 1, 50)
  println("First tree: " + inorder(tree1))
  val tree2 = createTree(3, 1, 50)
  println("Second tree: " + inorder(tree2))
  val resultTree = subtractTrees(tree1, tree2)
  println("Result tree: " + inorder(resultTree))






















}
