import scala.annotation.tailrec
// Karol Waliszewski

// Binary tree
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

// Random positive number
def getRandomNumber(range:(Int, Int)):Int = {
  if(range._1 <= 0 || range._2 <= 0)
    throw new Exception("Range have to be positive value.")

  if(range._2 < range._1)
    throw new Exception("Invalid range.")

  val rand = scala.util.Random
  rand.nextInt(range._2 - range._1 + 1) + range._1
}

// Check tree height
def treeHeight(tree: BT[Int]):Int =
  tree match {
    case Empty => 0
    case Node(_, left, right) => 1 + {
        val leftHeight = treeHeight(left)
        val rightHeight = treeHeight(right)

        if(leftHeight < rightHeight)
          rightHeight
        else
          leftHeight
    }
  }

// Count nodes in the tree
def countNodes(tree: BT[Int]):Int =
  tree match {
    case Empty => 0
    case Node(_, left, right) => 1 + countNodes(left) + countNodes(right)
  }


// Check if tree is full
def treeFull(tree: BT[Int]):Boolean = {
  countNodes(tree) == (Math.pow(2, treeHeight(tree)) - 1)
}

// 1) - 3pkt
def generateTree(depth: Int, range:(Int, Int)):BT[Int] = {
  if(depth <= 0)
    Empty
  else
    Node(getRandomNumber(range), generateTree(depth - 1, range), generateTree(depth - 1, range))
}

generateTree(-1, (0, 0))
generateTree(0, (10, 20))
generateTree(1, (10, 20))
generateTree(2, (10, 20))
generateTree(3, (10, 20))

// 2) - 3pkt
def subtractTrees(tree1: BT[Int], tree2: BT[Int]):BT[Int] = {
  if(!treeFull(tree1) || !treeFull(tree2))
    throw new Exception("Both trees have to be full")
  else if(treeHeight(tree1) != treeHeight(tree2))
    throw new Exception("Both trees have to have same height")
  else
    (tree1, tree2) match {
      case (Node(t1, t1l, t1r), Node(t2, t2l, t2r)) => Node(t1 - t2, subtractTrees(t1l, t2l), subtractTrees(t1r, t2r))
      case (_, _) => Empty
    }
}

var ex2Tree1 = generateTree(3, (10, 50))
var ex2Tree2 = generateTree(3, (40, 100))

subtractTrees(ex2Tree1, ex2Tree2)
subtractTrees(Empty, Empty)
//subtractTrees(Node(10, Empty, Empty), Node(10, Node(1,Empty, Empty), Empty))

// 3) - 4pkt
// DFS
def removeDuplicatesInTreesDFS(tree1: BT[Int], tree2: BT[Int]):(BT[Int], BT[Int]) =
  if(!treeFull(tree1) || !treeFull(tree2))
    throw new Exception("Both trees have to be full")
  else if(treeHeight(tree1) != treeHeight(tree2))
    throw new Exception("Both trees have to have same height")
  else
  (tree1, tree2) match {
    case (Node(t1, Empty, Empty), Node(t2, Empty, Empty)) => if(t1 == t2) (Empty, Empty) else (Node(t1, Empty, Empty),Node(t2, Empty, Empty))
    case (Node(t1, t1l, t1r), Node(t2, t2l, t2r)) => {
      val (t1Left, t2Left) = removeDuplicatesInTreesDFS(t1l, t2l)
      val (t1Right, t2Right) = removeDuplicatesInTreesDFS(t1r, t2r)

      if(t1 == t2 && t1Left == Empty && t2Left == Empty && t1Right == Empty && t2Right == Empty)
        (Empty, Empty)
      else
        (Node(if(t1 == t2) -1 else t1, t1Left, t1Right), Node(if(t1 == t2) -1 else t2, t2Left, t2Right))
    }
    case (_, _) => (Empty, Empty)
  }

// BFS
def removeDuplicatesInTreesBFS(tree1: BT[Int], tree2: BT[Int]):(BT[Int], BT[Int]) = {
  def removeDuplicatesInTreesBFSInner(queue: List[(BT[Int],BT[Int])]):List[(BT[Int],BT[Int])] =
    queue match {
      case Nil => Nil
      case (Empty, Empty)::tl => (Empty, Empty) :: removeDuplicatesInTreesBFSInner(tl)
      case (Node(val1,left1,right1),Node(val2,left2,right2))::tl => (Node(if(val1 == val2) -1 else val1, Empty, Empty), Node(if(val1 == val2) -1 else val2, Empty, Empty)) :: removeDuplicatesInTreesBFSInner(tl ::: List((left1,left2),(right1, right2)))
    }


  if(!treeFull(tree1) || !treeFull(tree2))
    throw new Exception("Both trees have to be full")
  else if(treeHeight(tree1) != treeHeight(tree2))
    throw new Exception("Both trees have to have same height")
  else
    removeDuplicatesInTreesBFSInner(List((tree1,tree2))).head
}

// Tests
val ex3Test1Tree1 = Node(3,Node(2,Node(4,Empty,Empty),Node(2,Empty,Empty)),Node(2,Node(1,Empty,Empty),Node(1,Empty,Empty)))
val ex3Test1Tree2 = Node(3,Node(2,Node(5,Empty,Empty),Node(2,Empty,Empty)),Node(2,Node(1,Empty,Empty),Node(1,Empty,Empty)))

removeDuplicatesInTreesDFS(ex3Test1Tree1, ex3Test1Tree2)
removeDuplicatesInTreesBFS(ex3Test1Tree1, ex3Test1Tree2)

val ex3Test2Tree1 = Node(2,Node(2,Node(2,Empty,Empty),Node(1,Empty,Empty)),Node(2,Node(1,Empty,Empty),Node(2,Empty,Empty)))
val ex3Test2Tree2 = Node(1,Node(2,Node(1,Empty,Empty),Node(1,Empty,Empty)),Node(1,Node(1,Empty,Empty),Node(1,Empty,Empty)))

removeDuplicatesInTreesDFS(ex3Test2Tree1, ex3Test2Tree2)
removeDuplicatesInTreesBFS(ex3Test2Tree1, ex3Test2Tree2)

val ex3Test3Tree1 = Node(2,Node(1, Empty, Empty), Node(2, Empty, Empty))
val ex3Test3Tree2 = Node(1,Node(3, Empty, Empty), Node(2, Empty, Empty))

removeDuplicatesInTreesDFS(ex3Test3Tree1, ex3Test3Tree2)
removeDuplicatesInTreesBFS(ex3Test3Tree1, ex3Test3Tree2)

removeDuplicatesInTreesDFS(Empty, Empty)
removeDuplicatesInTreesBFS(Empty, Empty)

// 4) - 5pkt
def eachNElement[A](list: LazyList[A], n:Int, m:Int): LazyList[A] = {
  if(n <= 0)
    throw new Exception("n value has to be positive.")

  def eachNElementInner(list: LazyList[A], k: Int): LazyList[A] = {
    if(m == k)
      LazyList()
    else
      list match {
        case value #:: tail => if(k % n == 0) value #:: eachNElementInner(tail, k + 1) else eachNElementInner(tail, k + 1)
        case _ => LazyList()
      }
  }

  eachNElementInner(list, 0)
}

// Tests
eachNElement(LazyList(5,6,3,2,1),2,3).toList
eachNElement(LazyList(5,6,3,2,1),2,4).toList
eachNElement(LazyList(1,2,3,4,5,6,7,8,9,10),2,8).toList
eachNElement(LazyList(),2,10).toList
eachNElement(LazyList(1,2,3,4,5,6,7,8,9,10),1,5).toList
eachNElement(LazyList(1,2,3,4,5,6,7,8,9,10),1,0).toList
eachNElement(LazyList(1,2,3,4,5,6,7,8,9,10),100,100).toList
//eachNElement(LazyList(1,2,3,4,5,6,7,8,9,10),0).toList
//eachNElement(LazyList(1,2,3,4,5,6,7,8,9,10),0).toList
eachNElement(LazyList.from(10),10,100).toList

// 5) - 5pkt
def ldzialanie[A]( operation: (A, A) => A)(l1: LazyList[A], l2: LazyList[A]): LazyList[A] =
  (l1, l2) match {
    case (hd1 #:: tl1, hd2 #:: tl2) => operation(hd1, hd2) #:: ldzialanie(operation)(tl1, tl2)
    case (l1, LazyList()) => l1
    case (LazyList(), l2) => l2
    case (_, _) => LazyList()
  }

// Tests
ldzialanie((a: Double, b:Double) => a + b)(LazyList(1.0, 2, 3, 4, 5), LazyList(1.0, 2, 3, -4, 5)).toList
ldzialanie((a: Double, b:Double) => a - b)(LazyList(1.0, 2, 3, 4, 5), LazyList(1.0, -2, 3, 4, 5)).toList
ldzialanie((a: Double, b:Double) => a / b)(LazyList(1.0, 2, 3), LazyList(1.0, 2, -3, -4, -5)).toList
ldzialanie((a: Double, b:Double) => a * b)(LazyList(1.0, -2, 3, -4, 5), LazyList(1.0, 2, 3)).toList

val addLists = ldzialanie((a: Double, b:Double) => a + b)(_,_)
addLists(LazyList(1,2,3), LazyList(-1,-2,-3)).toList