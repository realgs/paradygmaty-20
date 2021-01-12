import scala.util.Random

sealed trait BT[+Int]
case class Empty() extends BT[Nothing]
case class Node[Int](var elem: Int, var left: BT[Int], var right: BT[Int]) extends BT[Int]

object Functions {
  // Zadanie 1 (3pkt)
  def generateTree(n: Int, lower: Int, higher: Int): Node[Int] =
  {
    if (higher <= lower || n < 0)
      throw new IllegalArgumentException()
    val random = new Random()

    def generate(tree: Node[Int], n: Int): Unit = {
      if (n > 0)
        {
          tree.left = Node(random.nextInt(higher - lower) + lower, Empty(), Empty())
          tree.right = Node(random.nextInt(higher - lower) + lower, Empty(), Empty())
          generate(tree.left.asInstanceOf[Node[Int]], n - 1)
          generate(tree.right.asInstanceOf[Node[Int]], n - 1)
        }
    }

    if (n > 0) {
      val tree = Node(random.nextInt(higher - lower) + lower, Empty(), Empty())
      generate(tree, n)
      return tree
    }
    else return Node(random.nextInt(higher - lower) + lower, Empty(), Empty())
  }

  // Zadanie 2 (3pkt)
  def generateDifferTree(oldTree1: Node[Int], oldTree2: Node[Int]): Node[Int] =
  {
    // Złożoność pamięciowa: Trzy drzewa pełne o wysokości h. Głębokość stosu wywołań rekurencyjnych funkcji generate: h
    // Złożoność obliczeniowa: O(2^h)
    def generate(newTree: Node[Int], oldTree1: Node[Int], oldTree2: Node[Int]): Unit = {
      if (!oldTree1.left.isInstanceOf[Empty])
        {
          newTree.left = Node(oldTree1.left.asInstanceOf[Node[Int]].elem - oldTree2.left.asInstanceOf[Node[Int]].elem, Empty(), Empty())
          newTree.right = Node(oldTree1.right.asInstanceOf[Node[Int]].elem - oldTree2.right.asInstanceOf[Node[Int]].elem, Empty(), Empty())
          generate(newTree.left.asInstanceOf[Node[Int]], oldTree1.left.asInstanceOf[Node[Int]], oldTree2.left.asInstanceOf[Node[Int]])
          generate(newTree.right.asInstanceOf[Node[Int]], oldTree1.right.asInstanceOf[Node[Int]], oldTree2.right.asInstanceOf[Node[Int]])
        }
    }
    val newTree = Node(oldTree1.elem - oldTree2.elem, Empty(), Empty())
    generate(newTree, oldTree1, oldTree2)
    return newTree
  }

  // Zadanie 3 a) (1pkt)
  def generateNoDuplicatesTreeDFS(oldTree1: Node[Int], oldTree2: Node[Int]): (Node[Int], Node[Int]) =
    {
      // Złożoność pamięciowa: Dwa pełne drzewa o wysokości h. Głębokość stosu wywołań rekurencyjnych generate: h
      // Złożoność obliczeniowa: O(2^h)
      def generate(oldTree1: Node[Int], oldTree2: Node[Int]): Boolean =
        {
          if(!oldTree1.left.isInstanceOf[Empty])
            {
              val isDifferenceHere = oldTree1.elem != oldTree2.elem
              val isDifferenceLeft = generate(oldTree1.left.asInstanceOf[Node[Int]], oldTree2.left.asInstanceOf[Node[Int]])
              if (isDifferenceLeft && isDifferenceHere)
                {
                  oldTree1.elem = -1
                  oldTree2.elem = -1
                }
              else if (!isDifferenceLeft)
                {
                  oldTree1.left = Empty()
                  oldTree2.left = Empty()
                }

              val isDifferenceRight = generate(oldTree1.right.asInstanceOf[Node[Int]], oldTree2.right.asInstanceOf[Node[Int]])
              if (isDifferenceRight && isDifferenceHere)
              {
                oldTree1.elem = -1
                oldTree2.elem = -1
              }
              else if (!isDifferenceRight)
              {
                oldTree1.right = Empty()
                oldTree2.right = Empty()
              }
              return isDifferenceLeft | isDifferenceRight | isDifferenceHere
            }
          else
              return oldTree1.elem != oldTree2.elem
        }
      generate(oldTree1, oldTree2)
      return (oldTree1, oldTree2)
    }

  // Zadanie 3 b) (3pkt)
  def generateNoDuplicatesTreeBFS(oldTree1: Node[Int], oldTree2: Node[Int]): (Node[Int], Node[Int]) =
    {
      def generate(queue: List[(Node[Int], Node[Int])]): List[Boolean] =
      {
        queue match
          {
          case (tree1, tree2) :: tail =>
            val isDifferenceHere = tree1.elem != tree2.elem
            if(!tree1.left.isInstanceOf[Empty])
            {
              isDifferenceHere :: generate(tail ::: List((tree1.left.asInstanceOf[Node[Int]], tree2.left.asInstanceOf[Node[Int]]), (tree1.right.asInstanceOf[Node[Int]]
                , tree2.right.asInstanceOf[Node[Int]])))
            }
            else Nil
        }
      }
      def checkDifferencesForNodes(differences: Array[Boolean], bound: Int, treeHeight: Int): List[(Int, Int)] =
        {
          def checkForNode(i: Int, treeHeight: Int): Boolean =
            {
              if (treeHeight > 0)
                {
                  if(!differences(2 * i + 1) || !differences(2 * i + 2))
                  {
                    var flag = checkForNode(2 * i + 1, treeHeight - 1)
                    if (flag)
                      return true
                    flag = checkForNode(2 * i + 1, treeHeight - 1)
                    if (flag)
                      return true
                    else
                      return false
                  }
                  else
                      return true
                }
              else return false
            }
          var list = List[(Int, Int)]()
          for (i <- 0 to bound)
            {
              val differenceInChildren = checkForNode(i, treeHeight)
              if (!differenceInChildren && !differences(i))
                {
                  list = list.appended((i, 0))
                }
              else if (!differences(i) && differenceInChildren)
                {
                  list = list.appended((i, 1))
                }
            }
          return list
        }
      def changeNode(nodeNumber: Int, actionType: Int, tree1: Node[Int], tree2: Node[Int]): Unit =
        {
          @scala.annotation.tailrec
          def breath(queue: List[(BT[Int], String, BT[Int])], nodeNumber: Int): Unit =
            (queue, nodeNumber) match
            {
              case (Nil, _) =>
              case (h :: _, 0) =>
                if (actionType == 1)
                  h._1.asInstanceOf[Node[Int]].elem = -1
                else
                  if (h._2 == "left")
                    h._3.asInstanceOf[Node[Int]].left = Empty()
                  else
                    h._3.asInstanceOf[Node[Int]].right = Empty()
              case (h :: t, number) =>
                breath (t ::: List((h._1.asInstanceOf[Node[Int]].left, "left", h._1.asInstanceOf[Node[Int]]),
                  (h._1.asInstanceOf[Node[Int]].right, "right", h._1.asInstanceOf[Node[Int]])), number - 1)
            }
          breath(List((tree1.left, "left", tree1), (tree1.right, "right", tree1)), nodeNumber)
          breath(List((tree2.left, "left", tree2), (tree2.right, "right", tree2)), nodeNumber)
        }

      val differences = generate(List((oldTree1, oldTree2)))
      val treeHeight = (math.log(differences.length) / math.log(2)).toInt
      val nodesToChange = checkDifferencesForNodes(differences.toArray, math.pow(2, treeHeight - 1).toInt, treeHeight)
      for((i, j) <- nodesToChange)
        {
          changeNode(i, j, oldTree1, oldTree2)
        }
      return (oldTree1, oldTree2)
    }

  // Zadanie 4 (5pkt)
  def eachNElement [A](N: Int, M: Int, list: LazyList[A]): LazyList[A] =
    {
      def takeNElem(n: Int, m: Int, list: LazyList[A]): LazyList[A] =
        {
          (n, m, list) match
            {
            case (_, _, LazyList()) => LazyList()
            case (_, 0, _) => LazyList()
            case (0, m, h #:: t) => h #:: takeNElem(N - 1, m - 1, t)
            case (n, m, _ #:: t) => takeNElem(n - 1, m - 1, t)
          }
        }
      if (N > 0 && M > 0)
          takeNElem(0, M, list)
      else
        LazyList()
    }

  // Zadanie 5 (5pkt)
  def ldzialanie [A](first: LazyList[A], second: LazyList[A], operation: (A, A) => A): LazyList[A] =
    {
      def makeCalc(first: LazyList[A], second: LazyList[A]): LazyList[A] =
        {
          (first, second) match
            {
            case (LazyList(), LazyList()) => LazyList()
            case (LazyList(), h #:: t) => h #:: makeCalc(LazyList(), t)
            case (h #:: t, LazyList()) => h #:: makeCalc(t, LazyList())
            case (h1 #:: t1, h2 #:: t2) => operation(h1, h2) #:: makeCalc(t1, t2)
          }
        }
      makeCalc(first, second)
    }
}
