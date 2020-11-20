object Tasks123 {
    sealed trait BT[+A]
    case object Empty extends BT[Nothing]
    case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

    def count[A](tree: BT[A]): Int = {
        tree match {
            case Empty => 0
            case Node(_, left, right) => 1 + count(left) + count(right)
        }
    }

    def breadth[A](tree: BT[A]): List[A] = {
        def breadthRec[A](trees: List[BT[A]]): List[A] = {
            trees match {
                case Nil => List()
                case Empty::tl => breadthRec(tl)
                case Node(value, left, right)::tl => value :: breadthRec(tl ::: List(left, right))
            }
        }
        breadthRec(List(tree))
    }

    // Zad1 (3pkt)
    def generateNDepthTree(N: Int, min: Int, max: Int): BT[Int] = {
        def generateNDepthT(N: Int): BT[Int] = {
            if (N == 0) Empty
            else Node(
                scala.util.Random.nextInt(max - min + 1) + min,
                generateNDepthTree(N-1, min, max),
                generateNDepthTree(N-1, min, max))
        }
        if (N < 0) throw new Exception ("N cannot be negative")
        if (min > max) throw new Exception ("min cannot be greater than max")
        generateNDepthT(N)
    }

    def joinTrees[A](tree1: BT[A], tree2: BT[A], operation: (A, A) => A): BT[A] = {
        (tree1, tree2) match {
            case (Empty, Empty) => Empty
            case (Node(elem1, left1, right1), Node(elem2, left2, right2))
                => Node(operation(elem1, elem2), joinTrees(left1, left2, operation), joinTrees(right1, right2, operation))
            case _ => throw new Exception("trees must match to themselves")
        }
    }
    // Zad2 (3pkt)
    def subtractElements(tree1: BT[Int], tree2: BT[Int]): BT[Int] =
       joinTrees(tree1, tree2, (a: Int, b: Int) => a - b)

    // Zad3 (4pkt)
    def pruneTreesDepth(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) = {
        (tree1, tree2) match {
            case (Empty, Empty) => (Empty, Empty)
            case (Node(elem1, left1, right1), Node(elem2, left2, right2)) => {
                val left = pruneTreesDepth(left1, left2)
                val right = pruneTreesDepth(right1, right2)
                if (elem1 == elem2) {
                    (left, right) match {
                        case ((Empty, Empty), (Empty, Empty)) => (Empty, Empty)
                        case _ => (Node(-1, left._1, right._1), Node(-1, left._2, right._2))
                    }
                }
                else
                    (Node(elem1, left._1, right._1), Node(elem2, left._2, right._2))
            }
            case _ => throw new IllegalArgumentException("trees must match to themselves")
        }
    }

    def zipTreesWithBool[A](tree1: BT[A], tree2: BT[A], predicate: (A,A) => Boolean): BT[(A,A,Boolean)] = {
        (tree1, tree2) match {
            case (Empty, Empty) => Empty
            case (Node(elem1, left1, right1), Node(elem2, left2, right2)) =>
                Node((elem1, elem2, predicate(elem1, elem2)),
                    zipTreesWithBool(left1, left2, predicate),
                    zipTreesWithBool(right1, right2, predicate))
            case _ => throw new IllegalArgumentException("trees must match to themselves")
        }
    }

    def arrayToBT(arr: Array[(Int, Int, Boolean)], index: Int): (BT[Int], BT[Int]) = {
        if (index >= arr.length || arr(index)._1 == -2 || arr(index)._2 == -2)
            (Empty, Empty)
        else {
            val left = arrayToBT(arr, 1 + 2 * index)
            val right = arrayToBT(arr, 2 + 2 * index)
            (Node(arr(index)._1, left._1, right._1), Node(arr(index)._2, left._2, right._2))
        }
    }

    def pruneTreesBreadth(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) = {
        val bread = breadth(zipTreesWithBool(tree1, tree2, (a: Int, b: Int) => false))
        val arr = bread.toArray
        val len = arr.length
        var i = len / 2
        while (!arr(0)._3) {
            if (!arr(i)._3) {
                if ((1 + 2 * i >= len || arr(1 + 2 * i)._3)
                    && (2 + 2 * i >= len || arr(2 + 2 * i)._3)) {
                    if (arr(i)._1 == arr(i)._2) {
                        if ((1 + 2 * i >= len || arr(1 + 2 * i)._1 == -2)
                            && (2 + 2 * i >= len || arr(2 + 2 * i)._2 == -2)) {
                            arr(i) = (-2, -2, true)
                        }
                        else {
                            arr(i) = (-1, -1, true)
                        }
                    }
                    else {
                        arr(i) = (arr(i)._1, arr(i)._2, true)
                    }
                }
            }
            i = (i + 1) % len
        }
        arrayToBT(arr, 0)
    }
}
