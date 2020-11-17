package List4

import scala.annotation.tailrec

object L4Trees {

  sealed trait BT[+A]

  case object Empty extends BT[Nothing]

  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  //Zadanie 1(3pkt)
  @tailrec
  def createTree(depth: Int, rangeFrom: Int, rangeTo: Int): BT[Int] = {
    if (depth < 0) throw new Exception("Invalid data of depth")
    else {
      if (rangeFrom > rangeTo) createTree(depth, rangeTo, rangeFrom)
      else if (rangeFrom < 0 && rangeTo < 0) throw new Exception("The range have to start with positive number")
      else if (rangeFrom < 0) createTree(depth, 0, rangeTo)
      else {
        def createTreeIter(currentDepth: Int): BT[Int] = {
          if (currentDepth == depth) Empty
          else {
            Node(randomisation(rangeFrom, rangeTo), createTreeIter(currentDepth + 1), createTreeIter(currentDepth + 1))
          }
        }

        createTreeIter(0)
      }
    }
  }

  //funkcja pomocnicza losująca liczbe z przedziału
  def randomisation(from: Int, to: Int): Int = {
    val r = scala.util.Random
    r.nextInt(to + 1) + from
  }

  //funkcja pomocnicza do wyliczenia liczby węzłów w drzewie
  def countNodes(tree: BT[Int]): Int =
    tree match {
      case Empty => 0
      case Node(_, l, r) => 1 + countNodes(l) + countNodes(r)
    }

  //funkcja pomocnicza do sprawdzenia głębokości w drzewie
  @tailrec
  def checkDepth(tree: BT[Int], actualDepth: Int): Int =
    tree match {
      case Empty => actualDepth
      case Node(_, l, r) => checkDepth(l, actualDepth + 1)
    }

  //funkcja pomocnicza do sprawdzenia czy drzewo jest pełne
  def ifFullTree(tree: BT[Int]): Boolean =
    if (countNodes(tree) == Math.pow(2, checkDepth(tree, 0)) - 1) true
    else false

  //Funkcja pomocnicza do przedstawienia węzłów drzewa w liście za pomocą przejścia drzewa wszerz
  def breadthBT[A](bt: BT[A]): List[A] = {
    def breadthBTInner(queue: List[BT[A]]): List[A] =
      queue match {
        case Nil => Nil
        case Empty :: tail => breadthBTInner(tail)
        case Node(v, l, r) :: tail => v :: breadthBTInner(tail ::: List(l, r))
      }
    breadthBTInner(List(bt))
  }

  //Funkcja pomocnicza do przechodzenia drzewa w głąb
  def depthBT[A](bt: BT[A]): List[A] = {
    def depthBTInner(stack: List[BT[A]]): List[A] =
      stack match {
        case Nil => Nil
        case Empty :: tail => depthBTInner(tail)
        case Node(v, l, r) :: tail => v :: depthBTInner(List(l, r) ::: tail)
      }
    depthBTInner(List(bt))
  }

  //Zadanie 2(3pkt)
  def substractionNode(tree1: BT[Int], tree2: BT[Int]): BT[Int] =
    if (checkDepth(tree1, 0) != checkDepth(tree2, 0)) Empty
    else {
      (tree1, tree2) match {
        case (Empty, Empty) => Empty
        case (Node(v1, l1, r1), Node(v2, l2, r2)) => Node(v1 - v2, substractionNode(l1, l2), substractionNode(r1, r2))
      }
  }

  //Zadanie 3(1pkt - depthBT, 3pkt - breadthBT)
  val t: BT[Int] = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))
  val t2: BT[Int] = Node(1, Node(4, Empty, Empty), Node(3, Empty, Empty))
  val t3: BT[Int] = Node(1, Node(2, Node(4, Node(6, Empty, Empty), Node(7, Empty, Empty)), Node(5, Node(8, Empty, Empty), Node(9, Empty, Empty))), Node(3, Node(10, Node(12, Empty, Empty), Node(13, Empty, Empty)), Node(11, Node(14, Empty, Empty), Node(15, Empty, Empty))))
  val t4: BT[Int] = Node(1, Node(2, Node(4, Node(6, Empty, Empty), Node(7, Empty, Empty)), Node(5, Node(8, Empty, Empty), Node(9, Empty, Empty))), Node(3, Node(9, Node(12, Empty, Empty), Node(13, Empty, Empty)), Node(11, Node(13, Empty, Empty), Node(17, Empty, Empty))))
  val t5: BT[Int] = Node(1, Node(1, Node(-5,Empty, Empty), Node(-27, Empty, Empty)), Node(6, Node(18,Empty, Empty), Node(1, Empty, Empty)))
  val t6: BT[Int] = Node(8, Node(9, Node(6,Empty, Empty), Node(27, Empty, Empty)), Node(10, Node(19,Empty, Empty), Node(0, Empty, Empty)))
  val t7: BT[Int] = Node(1, Empty, Empty)
  val t8: BT[Int] = Node(8, Empty, Empty)

  //funkcja do zadania 3 za pomocą przejścia w głąb(1pkt)
  def repeatingNodesDepth(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) =
    if (!ifFullTree(tree1) || !ifFullTree(tree2) || checkDepth(tree1, 0) != checkDepth(tree2, 0)) throw new Exception("The tree isn't full tree or they don't have the same depth")
    else {
      (tree1, tree2) match {
        case (Empty, Empty) => (Empty, Empty)
        case (Node(v1, l1, r1), Node(v2, l2, r2)) => {
          val leftSubtree = repeatingNodesDepth(l1, l2)
          val rightSubtree = repeatingNodesDepth(r1, r2)

          if (v1 == v2 & leftSubtree._1 == Empty & rightSubtree._1 == Empty) (Empty, Empty)
          else if (v1 == v2) (Node(-1, leftSubtree._1, rightSubtree._1), Node(-1, leftSubtree._2, rightSubtree._2))
          else (Node(v1, leftSubtree._1, rightSubtree._1), Node(v2, leftSubtree._2, rightSubtree._2))
        }
      }
    }

  //funkcja pomocnicza sprawdzająca czy lista zawiera daną liczbę i zwracająca jej index
  @tailrec
  def checkIfContains(list: List[(Int, Int)], number: Int, index: Int): Int =
    if (list.isEmpty) -1
    else if (list.head._2 == number) index
    else checkIfContains(list.tail, number, index+1)

  //funkcja pomocnicza pozwalająca dostać wartość listy na odpowiednim indexie
  def getValueAt[T](list: List[T], index: Int): T = {
    @tailrec
    def getValueIter(list: List[T], index: Int, i: Int): T = {
      if (index == i) list.head
      else getValueIter(list.tail, index, i+1)
    }
    getValueIter(list, index, 0)
  }

  //funkcja pomocnicza przekształcająca listę (z wartością węzła i jego miejscem w drzewie) na drzewo
  def listToTree(actualNode: Int, numberOfNodes: Int, listOfNodes: List[(Int,Int)]): BT[Int] = {
    if (actualNode > numberOfNodes || checkIfContains(listOfNodes, actualNode, 0) == -1) return Empty
    Node(getValueAt(listOfNodes, checkIfContains(listOfNodes, actualNode, 0))._1, listToTree(actualNode*2, numberOfNodes, listOfNodes), listToTree(actualNode*2+1, numberOfNodes, listOfNodes))
  }

  //Funkcja do zadania 3 z zastosowaniem przejścia wszerz(3pkt)
  def repeatingNodesBreadth(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) = {
    if (!ifFullTree(tree1) || !ifFullTree(tree2) || checkDepth(tree1, 0) != checkDepth(tree2, 0)) throw new Exception("The tree isn't full tree or they don't have the same depth")
    else {
      @tailrec
      def createSubtreeQueueIter(queue: List[(BT[Int], BT[Int])], resultqueue: List[(BT[Int], BT[Int])]): List[(BT[Int], BT[Int])] =
        queue match {
          case Nil => resultqueue
          case (Empty, Empty) :: t => createSubtreeQueueIter(t, resultqueue)
          case (Node(_, l1, r1), Node(_, l2, r2)) :: t => createSubtreeQueueIter(t ::: List((l1, l2), (r1, r2)), if (l1 == Empty) resultqueue else resultqueue ::: List((l1, l2), (r1, r2)))
        }

      val queueOfTrees1 = createSubtreeQueueIter(List((tree1, tree2)), List((tree1, tree2)))
      val numberOfNodes = countNodes(tree1)

      @tailrec
      def repeatingNodesBreadthIter(numberOfNode: Int, ifDuplicate: Boolean, queueOfTrees: List[(BT[Int], BT[Int])], queue: List[(BT[Int], BT[Int])], actualCheckedTree: (BT[Int], BT[Int]), finalTree1: List[(Int, Int)], finalTree2: List[(Int, Int)]): (List[(Int, Int)], List[(Int, Int)]) = {
        if (queueOfTrees.length == 1) {
          actualCheckedTree match {
            case (Node(v1, _, _), Node(v2, _, _)) => if (v1 == v2) (finalTree1.reverse, finalTree2.reverse)
            else (((v1, numberOfNode) :: finalTree1).reverse, ((v2, numberOfNode) :: finalTree2).reverse)
          }
        }
        else if (queueOfTrees.length < 1) (finalTree1.reverse, finalTree2.reverse)
        else {
          actualCheckedTree match {
            case (Empty, Empty) => if (queue.isEmpty) repeatingNodesBreadthIter(numberOfNode + 1, false, queueOfTrees.tail, queueOfTrees.tail, queueOfTrees.tail.head, finalTree1, finalTree2)
            else repeatingNodesBreadthIter(numberOfNode, ifDuplicate, queueOfTrees, queue.tail, queue.tail.head, finalTree1, finalTree2)
            case (Node(v1, _, _), Node(v2, _, _)) => if (v1 == v2 & !queue.tail.isEmpty) repeatingNodesBreadthIter(numberOfNode, true, queueOfTrees, queue.tail, queue.tail.head, finalTree1, finalTree2)
            else if (v1 == v2 & queue.tail.isEmpty) repeatingNodesBreadthIter(numberOfNode + 1, false, queueOfTrees.tail, createSubtreeQueueIter(List((queueOfTrees.tail.head._1, queueOfTrees.tail.head._2)), List((queueOfTrees.tail.head._1, queueOfTrees.tail.head._2))), queueOfTrees.tail.head, finalTree1, finalTree2)
            else if (ifDuplicate) repeatingNodesBreadthIter(numberOfNode + 1, false, queueOfTrees.tail, createSubtreeQueueIter(List((queueOfTrees.tail.head._1, queueOfTrees.tail.head._2)), List((queueOfTrees.tail.head._1, queueOfTrees.tail.head._2))), queueOfTrees.tail.head, (-1, numberOfNode) :: finalTree1, (-1, numberOfNode) :: finalTree2)
            else repeatingNodesBreadthIter(numberOfNode + 1, false, queueOfTrees.tail, queueOfTrees.tail, queueOfTrees.tail.head, (v1, numberOfNode) :: finalTree1, (v2, numberOfNode) :: finalTree2)
          }
        }
      }
      val list = repeatingNodesBreadthIter(1, false, queueOfTrees1, queueOfTrees1, queueOfTrees1.head, Nil, Nil)
      (listToTree(1, numberOfNodes, list._1), listToTree(1, numberOfNodes, list._2))
    }
  }
}