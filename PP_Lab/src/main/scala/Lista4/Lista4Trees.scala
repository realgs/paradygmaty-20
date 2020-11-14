package Lista4

import scala.annotation.tailrec

object Lista4Trees extends App {
  // Tree
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]


  // Exceptions
  class WrongValueOfArguments extends Exception
  class WrongTreesLevel extends Exception
  class InternalException extends Exception


  // used to test exc 1
  def isTreeFull[A](tree: BT[A]): Boolean = {
    tree match {
      case Empty => true
      case Node(_, l, r) =>
        def innerIsTreeFull(leftTree: BT[A], rightTree: BT[A]): Boolean = {
          (leftTree, rightTree) match {
            case (Node(_, l1, r1), Node(_, l2, r2)) => innerIsTreeFull(l1, r1) && innerIsTreeFull(l2, r2)
            case (Empty, Empty) => true
            case (_, _) => false
          }
        }
        innerIsTreeFull(l, r)
    }
  }

  // used to test exc 1
  def isTreeDepthRight[A](tree: BT[A], expectedLevel: Int): Boolean = {
    tree match {
      case Empty => false
      case Node(_, l, r) =>
        def innerIsTreeDepthRight(leftTree: BT[A], rightTree: BT[A], level: Int): Boolean = {
          (leftTree, rightTree) match {
            case (Empty, Empty) => expectedLevel == 0
            case (Node(_, l1, r1), Node(_, l2, r2)) => innerIsTreeDepthRight(l1, r1, level-1) == innerIsTreeDepthRight(l2, r2, level-1)
            case (_, _) => false
          }
        }
        innerIsTreeDepthRight(l, r, expectedLevel)
    }
  }

  // used to test trees
  def inorder[A](tree:BT[A]):List[A] =
    tree match {
      case Node(v,l,r) => inorder(l) ::: v :: inorder(r)
      case Empty => Nil
  }

  // used in exc 2 and 3
  def chceckIfMatch[A](leftBT: BT[A], rightBT: BT[A]): Boolean = {
    if(isTreeFull(leftBT) && isTreeFull(rightBT)) {
      @tailrec
      def compareLevels(leftSubTree: BT[A], rightSubTree: BT[A]): Boolean =
        (leftSubTree, rightSubTree) match{
          case (Node(_, _, _), Empty) => false
          case (Empty, Node(_, _, _)) => false
          case (Empty, Empty) => true
          case (Node(_, left1, _), Node(_, left2, _)) => compareLevels(left1, left2) //skoro drzewa sa pelne to wystarczy ze sprawdze 1 sciezke
        }
      compareLevels(leftBT, rightBT)
    }
    else
      false
  }

  //////////////////////////////////////////////////zadania////////////////////////////////////////////////////////////////

  // zadanie 1 (3pkt)
  // dla amountOfLevels == 0 zwraca drzewo z samym korzeniem z wartoscia - potomkami korzenia sa Empty
  def createTree(amountOfLevels: Int, leftInterval: Int, rightInterval: Int): BT[Int] = {
    if (amountOfLevels < 0 || rightInterval < leftInterval)
      throw new WrongValueOfArguments
    else {
      val randomInt = scala.util.Random
      def innerCreateTree(levels: Int): BT[Int] = {
        if(levels == (-1))
          Empty
        else
          Node(randomInt.nextInt(rightInterval-leftInterval+1)+leftInterval, innerCreateTree(levels-1), innerCreateTree(levels-1))
      }
      innerCreateTree(amountOfLevels)
    }
  }


  // zadanie 2 (3pkt)
  // zlozonosc obliczeniowa - liniowa: O(n) - operacje trzeba wykonac dla kazdego wezla
  def subtractTrees(leftTree: BT[Int], rightTree: BT[Int]): BT[Int] = {
    if (!chceckIfMatch(leftTree, rightTree))
      throw new WrongTreesLevel
    else {
      (leftTree, rightTree) match {
        case (Node(value1, left1, right1), Node(value2, left2, right2)) => Node(value1 - value2, subtractTrees(left1, left2), subtractTrees(right1, right2))
        case (_, _) => Empty
      }
    }
  }


  // zadanie 3 (wgłab 1 pkt, wszerz 3 pkt)
  // wglab DFS
  // zlozonosc obliczeniowa - liniowa: O(n) - operacje trzeba wykonac dla kazdego wezla
  def compareTreesDFS(leftTree: BT[Int], rightTree: BT[Int]): (BT[Int], BT[Int]) = {
    if (!chceckIfMatch(leftTree, rightTree))
      throw new WrongTreesLevel
    else {
      def innerCompare(leftSubTree: BT[Int], rightSubTree: BT[Int]): (BT[Int], BT[Int]) =
        (leftSubTree, rightSubTree) match {
          case (Node(valL, Empty, Empty), Node(valR, Empty, Empty)) =>
            if(valL == valR)
              (Empty, Empty)
            else
              (Node(valL, Empty, Empty), Node(valR, Empty, Empty))

          case (Node(valL, left1, right1), Node(valR, left2, right2)) =>
            val resultLeftSubTrees = innerCompare(left1, left2)
            val resultRightSubTrees = innerCompare(right1, right2)
            if(valL != valR)
              (Node(valL, resultLeftSubTrees._1, resultRightSubTrees._1), Node(valR, resultLeftSubTrees._2, resultRightSubTrees._2))
            else {
              if(resultLeftSubTrees._1 == Empty && resultRightSubTrees._1 == Empty )
                (Empty, Empty) // wystarczy sprawdzic dla 1 drzewa wyniki lewego i prawego poddrzewa
              else
                (Node(-1, resultLeftSubTrees._1, resultRightSubTrees._1), Node(-1, resultLeftSubTrees._2, resultRightSubTrees._2))
            }
          case (_, _) => throw new InternalException
        }
      innerCompare(leftTree, rightTree)
    }
  }


  //wszerz BFS
  def ifSubTreesEqualsBFS(left: BT[Int], right: BT[Int]) : Boolean= {
    @tailrec
    def innerEquals(leftTree: List[BT[Int]], rightTree: List[BT[Int]]): Boolean =
      (leftTree, rightTree) match {
        case (Nil, Nil) => true
        case (Empty::tailL, Empty::tailR) => innerEquals(tailL, tailR)
        case (Node(valL, left1, right1)::tailL, Node(valR, left2, right2)::tailR) =>
          if(valL == valR)
            innerEquals(tailL:::List(left1, right1), tailR:::List(left2, right2))
          else
            false
        case(_, _) => false
      }
    innerEquals(List(left), List(right))
  }

  // zlozonosc obliczeniowa:
  // - w najlepszym przypadku O(n): np. drzewa identyczne lub rozniace sie tylko korzeniem
  // - w najgorszym O(n^2): np. drzewa z wszystkimi elementami roznymi (dla kazdego wezla wywolywane bedzie ifSubTreesEqualsBFS() o zlozonosci O(n))
  //   kub takie gdzie wezly rozniace sie beda blisko liscie i daleko od korzenia (pod katem glebokosci)
  // usprawnienie - jezeli wezly maja taka sama wartosc, ale ich rodzice maja rozne wartosci, to wezlowi mozemy przypisac od razu -1 (nie dotyczy lisci) bez sprawdzania ifSubTreesEqualsBFS()
  // ze wzglebdu na budowe algorytmu
  def compareTreesBFS(leftTree: BT[Int], rightTree: BT[Int]): (BT[Int], BT[Int]) = {
    if (!chceckIfMatch(leftTree, rightTree))
      throw new WrongTreesLevel
    else {
      def innerCompare(firstTree: BT[Int], secondTree: BT[Int], firstTreeParentVal: Int, secondTreeParentVal: Int): (BT[Int], BT[Int]) = {
        (firstTree, secondTree) match {
          //doszlismy do lisci
          case (Node(firstVal, Empty, Empty), Node(secondVal, Empty, Empty)) =>
            if (firstVal == secondVal) {
              (Empty, Empty)
            }
            else {
              (Node(firstVal, Empty, Empty), Node(secondVal, Empty, Empty))
            }
          // doszlismy do zwyklego wezla
          case (Node(firstVal, firstLeftSubTree, firstRightSubTree), Node(secondVal, secondLeftSubTree, secondRightSubTree)) =>
            if (firstVal == secondVal) {
              if (firstTreeParentVal == secondTreeParentVal) {
                (ifSubTreesEqualsBFS(firstLeftSubTree, secondLeftSubTree), ifSubTreesEqualsBFS(firstRightSubTree, secondRightSubTree)) match {
                  // obydwa poddrzewa sa rowne
                  case(true, true) => (Node(-1, Empty, Empty), Node(-1, Empty, Empty))
                  // lewe poddrzewa sa rowne, prawe sie roznia
                  case(true, false) =>
                    val (firstTreeRightSubTree, secondTreeRightSubtree) = innerCompare(firstRightSubTree, secondRightSubTree, -1, -1)
                    (Node(-1, Empty, firstTreeRightSubTree), Node(-1, Empty, secondTreeRightSubtree))
                  // lewe poddrzewa sa rozne, prawe rowne
                  case (false, true) =>
                    val (firstTreeLeftSubTree, secondTreeLeftSubTree) = innerCompare(firstLeftSubTree, secondLeftSubTree, -1, -1)
                    (Node(-1, firstTreeLeftSubTree, Empty), Node(-1, secondTreeLeftSubTree, Empty))
                  // obydwa poddrzewa sa rozne
                  case (false, false) =>
                    val (firstTreeLeftSubTree, secondTreeLeftSubTree) = innerCompare(firstLeftSubTree, secondLeftSubTree, -1, -1)
                    val (firstTreeRightSubTree, secondTreeRightSubtree) = innerCompare(firstRightSubTree, secondRightSubTree, -1, -1)
                    (Node(-1, firstTreeLeftSubTree, firstTreeRightSubTree), Node(-1, secondTreeLeftSubTree, secondTreeRightSubtree))
                }
              }
              else {
                val (firstTreeLeftSubTree, secondTreeLeftSubTree) = innerCompare(firstLeftSubTree, secondLeftSubTree, -1, -1)
                val (firstTreeRightSubTree, secondTreeRightSubtree) = innerCompare(firstRightSubTree, secondRightSubTree, -1, -1)
                (Node(-1, firstTreeLeftSubTree, firstTreeRightSubTree), Node(-1, secondTreeLeftSubTree, secondTreeRightSubtree))
              }
            }
            else {
              (ifSubTreesEqualsBFS(firstLeftSubTree, secondLeftSubTree), ifSubTreesEqualsBFS(firstRightSubTree, secondRightSubTree)) match {
                // obydwa poddrzewa sa rowne
                case(true, true) => (Node(firstVal, Empty, Empty), Node(secondVal, Empty, Empty))
                // lewe poddrzewa sa rowne, prawe sie roznia
                case(true, false) =>
                  val (firstTreeRightSubTree, secondTreeRightSubtree) = innerCompare(firstRightSubTree, secondRightSubTree, firstVal, secondVal)
                  (Node(firstVal, Empty, firstTreeRightSubTree), Node(secondVal, Empty, secondTreeRightSubtree))
                // lewe poddrzewa sa rozne, prawe rowne
                case (false, true) =>
                  val (firstTreeLeftSubTree, secondTreeLeftSubTree) = innerCompare(firstLeftSubTree, secondLeftSubTree, firstVal, secondVal)
                  (Node(firstVal, firstTreeLeftSubTree, Empty), Node(firstVal, secondTreeLeftSubTree, Empty))
                // obydwa poddrzewa sa rozne
                case (false, false) =>
                  val (firstTreeLeftSubTree, secondTreeLeftSubTree) = innerCompare(firstLeftSubTree, secondLeftSubTree, firstVal, secondVal)
                  val (firstTreeRightSubTree, secondTreeRightSubtree) = innerCompare(firstRightSubTree, secondRightSubTree, firstVal, secondVal)
                  (Node(firstVal, firstTreeLeftSubTree, firstTreeRightSubTree), Node(secondVal, secondTreeLeftSubTree, secondTreeRightSubtree))
              }
            }
        }
      }
      innerCompare(leftTree, rightTree, 0, 0)
    }
  }

  /*def compareTreesBFS(leftTree: BT[Int], rightTree: BT[Int]): (BT[Int], BT[Int]) = {
    if (!chceckIfMatch(leftTree, rightTree))
      throw new WrongTreesLevel
    else {
      def innerCompare(left: BT[Int], right: BT[Int]): (BT[Int], BT[Int]) = {
        val Node(firstVal, firstLeftSubTree, firstRightSubTree) = left
        val Node(secondVal, secondLeftSubTree, secondRightSubTree) = right

       (ifSubTreesEqualsBFS(firstLeftSubTree, secondLeftSubTree), ifSubTreesEqualsBFS(firstRightSubTree, secondRightSubTree)) match {
          // obydwa poddrzewa sa rowne
          case(true, true) => if (firstVal == secondVal)
                                (Empty, Empty)
                              else
                                (Node(firstVal, Empty, Empty), Node(secondVal, Empty, Empty))
          // lewe poddrzewa sa rowne, prawe sie roznia
          case(true, false) =>
            val (leftSubTree, rightSubTree) = innerCompare(firstRightSubTree, secondRightSubTree)
            if (firstVal == secondVal)
              (Node(-1, Empty, leftSubTree), Node(-1, Empty, rightSubTree))
            else
              (Node(firstVal, Empty, leftSubTree), Node(secondVal, Empty, rightSubTree))
          // lewe poddrzewa sa rozne, prawe rowne
          case (false, true) =>
            val (leftSubTree, rightSubTree) = innerCompare(firstLeftSubTree, secondLeftSubTree)
            if (firstVal == secondVal)
              (Node(-1, leftSubTree, Empty), Node(-1, rightSubTree, Empty))
            else
              (Node(firstVal, leftSubTree, Empty), Node(secondVal, rightSubTree, Empty))

          // obydwa poddrzewa sa rozne
          case (false, false) =>
            val (leftSubTreeLeft, rightSubTreeLeft) = innerCompare(firstLeftSubTree, secondLeftSubTree)
            val (leftSubTreeRight, rightSubTreeRight) = innerCompare(firstRightSubTree, secondRightSubTree)
            if (firstVal == secondVal)
              (Node(-1, leftSubTreeLeft, leftSubTreeRight), Node(-1, rightSubTreeLeft, rightSubTreeRight))
            else
              (Node(firstVal, leftSubTreeLeft, leftSubTreeRight), Node(secondVal, rightSubTreeLeft, rightSubTreeRight))
        }
      }
      innerCompare(leftTree, rightTree)
    }
  }*/

}
