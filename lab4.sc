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

// 1) - 3pkt
def generateTree(depth: Int, range:(Int, Int)):BT[Int] = {
  if(depth <= 0)
    Empty
  else if(depth == 1)
    Node(getRandomNumber(range), Empty, Empty)
  else
    Node(getRandomNumber(range), generateTree(depth - 1, range), generateTree(depth - 1, range))
}

generateTree(-1, (0, 0))
generateTree(0, (10, 20))
generateTree(1, (10, 20))
generateTree(2, (10, 20))
generateTree(3, (10, 20))

// 2) - 3pkt
def subtractTrees(tree1: BT[Int], tree2: BT[Int]):BT[Int] =
  (tree1, tree2) match {
    case (Node(t1, t1l, t1r), Node(t2, t2l, t2r)) => Node(t1 - t2, subtractTrees(t1l, t2l), subtractTrees(t1r, t2r))
    case (_, _) => Empty
  }

var ex2Tree1 = generateTree(3, (10, 50))
var ex2Tree2 = generateTree(3, (40, 100))

subtractTrees(ex2Tree1, ex2Tree2)

// 3) - 4pkt
// DFS
def removeDuplicatesInTreesDFS(tree1: BT[Int], tree2: BT[Int]):(BT[Int], BT[Int]) =
  (tree1, tree2) match {
    case (Node(t1, Empty, Empty), Node(t2, Empty, Empty)) => if(t1 == t2) (Empty, Empty) else (Node(t1, Empty, Empty),Node(t2, Empty, Empty))
    case (Node(t1, t1l, t1r), Node(t2, t2l, t2r)) => {
      val (t1Left, t2Left) = removeDuplicatesInTreesDFS(t1l, t2l)
      val (t1Right, t2Right) = removeDuplicatesInTreesDFS(t1r, t2r)

      if(t1Left == Empty && t2Left == Empty && t1Right == Empty && t2Right == Empty)
        (Empty, Empty)
      else
        (Node(if(t1 == t2) -1 else t1, t1Left, t1Right), Node(if(t1 == t2) -1 else t2, t2Left, t2Right))
    }
    case (_, _) => (Empty, Empty)
  }

// BFS
def removeDuplicatesInTreesBFS(tree1: BT[Int], tree2: BT[Int]):(BT[Int], BT[Int]) =
  (tree1, tree2) match {
    case (Node(t1, t1l, t1r), Node(t2, t2l, t2r)) => {
      @tailrec
      def breadthBTInner(heap: List[(BT[Int],BT[Int])], res: (BT[Int],BT[Int])):(BT[Int],BT[Int]) = {
        heap match {
          case (Empty, Empty)::tl => breadthBTInner(tl, res)
          case (Node(t1, t1l, t1r), Node(t2, t2l, t2r))::tl => breadthBTInner((t1l, t2l)::(t1r, t2r)::tl, (Empty, Empty))
          case _ => res
        }
      }
      breadthBTInner(List((Node(t1, t1l, t1r), Node(t2, t2l, t2r))), (Empty,Empty))
    }
    case (_, _) => (Empty, Empty)
  }

// Test
val ex3Tree1 = Node(2,Node(2,Node(2,Empty,Empty),Node(2,Empty,Empty)),Node(2,Node(1,Empty,Empty),Node(1,Empty,Empty)))
val ex3Tree2 = Node(1,Node(2,Node(1,Empty,Empty),Node(2,Empty,Empty)),Node(2,Node(1,Empty,Empty),Node(1,Empty,Empty)))

removeDuplicatesInTreesDFS(ex3Tree1, ex3Tree2)
removeDuplicatesInTreesBFS(ex3Tree1, ex3Tree2)

// 4) - 5pkt
def eachNElement[A](list: LazyList[A], n:Int): LazyList[A] = {
  if(n <= 0)
    throw new Exception("n value has to be positive.")

  def eachNElementInner(list: LazyList[A], k: Int): LazyList[A] =
    list match {
      case value #:: tail => if(k % n == 0) value #:: eachNElementInner(tail, k + 1) else eachNElementInner(tail, k + 1)
      case _ => LazyList()
    }

  eachNElementInner(list, 0)
}

// Tests
eachNElement(LazyList(1,2,3,4,5,6,7,8,9,10),2).toList
eachNElement(LazyList(),2).toList
eachNElement(LazyList(1,2,3,4,5,6,7,8,9,10),1).toList
eachNElement(LazyList(1,2,3,4,5,6,7,8,9,10),100).toList
//eachNElement(LazyList(1,2,3,4,5,6,7,8,9,10),0).toList
//eachNElement(LazyList(1,2,3,4,5,6,7,8,9,10),0).toList
eachNElement(LazyList.from(10),10).take(10).toList

// 5) - 5pkt
def ldzialanie[A](l1: LazyList[A], l2: LazyList[A], operation: (A, A) => A): LazyList[A] =
  (l1, l2) match {
    case (hd1 #:: tl1, hd2 #:: tl2) => operation(hd1, hd2) #:: ldzialanie(tl1, tl2, operation)
    case (l1, LazyList()) => l1
    case (LazyList(), l2) => l2
    case (_, _) => LazyList()
  }

// Tests
ldzialanie(LazyList(1.0, 2, 3, 4, 5), LazyList(1.0, 2, 3, -4, 5), (a: Double, b:Double) => a + b).toList
ldzialanie(LazyList(1.0, 2, 3, 4, 5), LazyList(1.0, -2, 3, 4, 5), (a: Double, b:Double) => a - b).toList
ldzialanie(LazyList(1.0, 2, 3), LazyList(1.0, 2, -3, -4, -5), (a: Double, b:Double) => a / b).toList
ldzialanie(LazyList(1.0, -2, 3, -4, 5), LazyList(1.0, 2, 3), (a: Double, b:Double) => a * b).toList