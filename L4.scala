import scala.util.Random

sealed trait Tree[+A]
case object Leaf extends Tree[Nothing]
case class Node[+A](elem: A, left: Tree[A], right: Tree[A]) extends Tree[A]

//zadanie 1 (3pkt)
def generateTree(depth: Int, range: (Int, Int)): Tree[Int] =
  if(depth < -1) throw new Exception("Invalid depth")
  else if(range._2 < range._1) throw new Exception("Invalid range")
  else if(depth == -1) Leaf
  else{
    val value = Random.between(range._1, range._2 + 1)
    Node(value, generateTree(depth - 1, range), generateTree(depth - 1, range))
  }

generateTree(3, (1,5))
generateTree(2, (1,1))
generateTree(1, (2,4))
generateTree(0, (3,5)) //only root

//zadanie 2 (3pkt)
def treesDiff(tree1: Tree[Int], tree2: Tree[Int]): Tree[Int] =
  (tree1, tree2) match{
    case (Leaf, _) => Leaf
    case (Node(val1, l1, r1), Node(val2, l2, r2)) => Node(val1 - val2, treesDiff(l1, l2), treesDiff(r1, r2))
  }

val t1 = Node(1, Node(4, Leaf, Node(3, Leaf, Leaf)),Leaf)
val t2 = Node(5, Node(1, Leaf, Node(3, Leaf, Leaf)),Leaf)
treesDiff(t1, t2) == Node(-4,Node(3,Leaf,Node(0,Leaf,Leaf)),Leaf)
val t3 = Node(5, Node(1, Leaf, Node(3, Leaf, Leaf)),Leaf)
treesDiff(t3, t3) == Node(0,Node(0,Leaf,Node(0,Leaf,Leaf)),Leaf)
val t4 = Leaf
treesDiff(t4, t4) == Leaf


//zadanie 3 (4pkt)
def dfsTreeDiff(tree1: Tree[Int], tree2: Tree[Int]): (Tree[Int], Tree[Int]) = {
  def postorder(tree1: Tree[Int], tree2: Tree[Int]): (Tree[Int], Tree[Int], Boolean) = {
    (tree1, tree2) match {
      case (Leaf, Leaf) => (Leaf, Leaf, false)
      case (Node(v1, l1, r1), Node(v2, l2, r2)) => {

        // ancestors info
        val (left1, left2, isLeftChildDiff) = postorder(l1, l2)
        val (right1, right2, isRightChildDiff) = postorder(r1, r2)

        // current node info
        val nodesEquality = v1 == v2

        if ((isLeftChildDiff || isRightChildDiff) && nodesEquality) // if any of ancestors are different and current nodes are the same, then -1
          (Node(-1, left1, right1), Node(-1, left2, right2), true)
        else if (isLeftChildDiff || isRightChildDiff) // if any of ancestors are different and current nodes are diff, then their values are saved
          (Node(v1, left1, right1), Node(v2, left2, right2), true)
        else if (!nodesEquality) // if ancestors are the same but current nodes are different, their values are saved
          (Node(v1, left1, right1), Node(v2, left2, right2), true)
        else // if ancestors are the same either current nodes, this branch is being deleted
          (Leaf, Leaf, false)
      }
    }
  }
  val (returnTree1, returnTree2, _) = postorder(tree1, tree2)
  (returnTree1, returnTree2)
}

val test1 = Node(1, Node(4, Leaf, Node(3, Leaf, Leaf)),Leaf)
val test2 = Node(1, Node(4, Leaf, Node(2, Leaf, Leaf)),Leaf)
dfsTreeDiff(test1, test2) == (Node(-1,Node(-1,Leaf,Node(3,Leaf,Leaf)),Leaf),Node(-1,Node(-1,Leaf,Node(2,Leaf,Leaf)),Leaf))

val test3 = Node(1, Node(3, Leaf, Node(4, Leaf, Leaf)),Node(2, Node(5, Leaf, Leaf), Leaf))
val test4 = Node(1, Node(3, Leaf, Node(4, Leaf, Leaf)),Node(2, Node(4, Leaf, Leaf), Leaf))
dfsTreeDiff(test3, test4) == (Node(-1,Leaf,Node(-1,Node(5,Leaf,Leaf),Leaf)),Node(-1,Leaf,Node(-1,Node(4,Leaf,Leaf),Leaf)))

val test5 = Node(1, Leaf, Leaf)
val test6 = Node(2, Leaf, Leaf)
dfsTreeDiff(test5, test6) == (Node(1,Leaf,Leaf),Node(2,Leaf,Leaf))

val test7 = Node(1, Leaf, Leaf)
val test8 = Node(1, Leaf, Leaf)
dfsTreeDiff(test7, test8) == (Leaf, Leaf)

val test9 = Node(1, Node(2, Leaf, Leaf), Node(3, Leaf, Leaf))
val test10 = Node(1, Node(2, Leaf, Leaf), Node(100, Leaf, Leaf))
dfsTreeDiff(test9, test10) == (Node(-1, Leaf, Node(3, Leaf, Leaf)), Node(-1, Leaf, Node(100, Leaf, Leaf)))

val test11 = Node(1, Node(3, Leaf, Node(4, Leaf, Leaf)),Node(2, Node(5, Leaf, Leaf), Leaf))
val test12 = Node(1, Node(3, Leaf, Node(4, Leaf, Leaf)),Node(9, Node(4, Leaf, Leaf), Leaf))
dfsTreeDiff(test3, test4) == (Node(-1,Leaf,Node(-1,Node(5,Leaf,Leaf),Leaf)),Node(-1,Leaf,Node(-1,Node(4,Leaf,Leaf),Leaf)))

val test13 = Node(1, Node(3, Leaf, Node(4, Leaf, Leaf)),Node(2, Node(5, Leaf, Leaf), Leaf))
val test14 = Node(1, Node(3, Leaf, Node(4, Leaf, Leaf)),Node(9, Node(4, Leaf, Leaf), Leaf))
dfsTreeDiff(test12, test13) == (Node(-1,Leaf,Node(9,Node(4,Leaf,Leaf),Leaf)),Node(-1,Leaf,Node(2,Node(5,Leaf,Leaf),Leaf)))

val test15 = Node(2, Leaf, Node(3, Leaf, Leaf))
val test16 = Node(4, Leaf, Node(5, Leaf, Leaf))
dfsTreeDiff(test15, test16) == (Node(2,Leaf,Node(3,Leaf,Leaf)),Node(4,Leaf,Node(5,Leaf,Leaf)))

val test17 = Node(1, Leaf, Node(2, Node(9, Leaf, Leaf), Node(9, Leaf, Leaf)))
val test18 = Node(1, Leaf, Node(1, Node(8, Leaf, Leaf), Node(9, Leaf, Leaf)))
dfsTreeDiff(test17, test18) == (Node(-1, Leaf, Node(2, Node(9, Leaf, Leaf), Leaf)), Node(-1, Leaf, Node(1, Node(8, Leaf, Leaf), Leaf)))


def bfsTreeDiff(tree1: Tree[Int], tree2: Tree[Int]): (Tree[Int], Tree[Int]) = {
  def bfsHelper(queue: List[(Tree[Int], Tree[Int])]): Boolean =
    queue match {
      case Nil => true
      case (Node(v1, l1, r1), Node(v2, l2, r2)) :: tail => {
        if (v1 == v2) true && bfsHelper(tail ::: List((l1, l2)) ::: List((r1, r2)))
        else false
      }
      case (Leaf, Leaf) :: tail => true && bfsHelper(tail)
    }

  def treesTupleCons(tree1: Tree[Int], tree2: Tree[Int]): (Tree[Int], Tree[Int]) = {
    (tree1, tree2) match {
      case (Leaf, Leaf) => (Leaf, Leaf)
      case (Node(v1, l1, r1), Node(v2, l2, r2)) =>
        if (bfsHelper(List((tree1, tree2)))) // all nodes are the same, do not check under
          (Leaf, Leaf)
        else if (v1 == v2) { // some nodes are different and current nodes values are different
          val (lAncestor1, lAncestor2) = treesTupleCons(l1, l2)
          val (rAncestor1, rAncestor2) = treesTupleCons(r1, r2)
          (Node(-1, lAncestor1, rAncestor1), Node(-1, lAncestor2, rAncestor2))
        }
        else { // some nodes are different and current nodes values are the same
          val (lAncestor1, lAncestor2) = treesTupleCons(l1, l2)
          val (rAncestor1, rAncestor2) = treesTupleCons(r1, r2)
          (Node(v1, lAncestor1, rAncestor1), Node(v2, lAncestor2, rAncestor2))
        }
    }
  }
  treesTupleCons(tree1, tree2)
}

bfsTreeDiff(test1, test2) == (Node(-1,Node(-1,Leaf,Node(3,Leaf,Leaf)),Leaf),Node(-1,Node(-1,Leaf,Node(2,Leaf,Leaf)),Leaf))
bfsTreeDiff(test3, test4) == (Node(-1,Leaf,Node(-1,Node(5,Leaf,Leaf),Leaf)),Node(-1,Leaf,Node(-1,Node(4,Leaf,Leaf),Leaf)))
bfsTreeDiff(test5, test6) == (Node(1,Leaf,Leaf),Node(2,Leaf,Leaf))
bfsTreeDiff(test7, test8) == (Leaf, Leaf)
bfsTreeDiff(test9, test10) == (Node(-1, Leaf, Node(3, Leaf, Leaf)), Node(-1, Leaf, Node(100, Leaf, Leaf)))
bfsTreeDiff(test3, test4) == (Node(-1,Leaf,Node(-1,Node(5,Leaf,Leaf),Leaf)),Node(-1,Leaf,Node(-1,Node(4,Leaf,Leaf),Leaf)))
bfsTreeDiff(test12, test13) == (Node(-1,Leaf,Node(9,Node(4,Leaf,Leaf),Leaf)),Node(-1,Leaf,Node(2,Node(5,Leaf,Leaf),Leaf)))
bfsTreeDiff(test15, test16) == (Node(2,Leaf,Node(3,Leaf,Leaf)),Node(4,Leaf,Node(5,Leaf,Leaf)))
bfsTreeDiff(test17, test18) == (Node(-1, Leaf, Node(2, Node(9, Leaf, Leaf), Leaf)), Node(-1, Leaf, Node(1, Node(8, Leaf, Leaf), Leaf)))


//zadanie 4 (5pkt)
def eachNElement[A](lxs: LazyList[A], n: Int, m: Int): LazyList[A] ={
  def helper[A](lxs: LazyList[A],n: Int, iter: Int): LazyList[A] =
    lxs match{
      case LazyList() => LazyList()
      case lh #:: lt => if(iter >= m) LazyList()
      else if(iter % n == 0 ) lh #:: helper(lt, n, iter + 1)
      else helper(lt, n, iter + 1)
    }
  if(n == 0) throw new Exception("Division by 0")
  helper(lxs, n, 0)
}

eachNElement(LazyList.from(1), 3, 10).take(999).toList == List(1, 4, 7, 10)
eachNElement(5 #:: 4 #:: 3 #:: 2 #:: 1 #:: LazyList(), 2, 3).take(999).toList == List(5, 3)
eachNElement(5 #:: 4 #:: 3 #:: 2 #:: 1 #:: LazyList(), 2, 4).take(999).toList == List(5, 3)
eachNElement(LazyList.from(1), 2, 5).take(999).toList == List(1, 3, 5)
eachNElement(LazyList.from(2), 2, 5).take(999).toList == List(2, 4, 6)
eachNElement(LazyList.from(0), 10, 20).take(999).toList == List(0, 10)
eachNElement(LazyList.from(0), 10, 21).take(999).toList == List(0, 10, 20)
eachNElement('a' #:: 'b' #:: 'c' #:: 'd' #:: 'f' #:: LazyList(), 3, 4).take(999).toList == List('a', 'd')
eachNElement('a' #:: 'b' #:: 'c' #:: 'd' #:: 'f' #:: LazyList(), 1, 2).take(999).toList == List('a', 'b')
eachNElement(1.1 #:: 2.2 #:: 3.3 #:: 4.4 #:: 5.5 #:: LazyList(), 2, 10).take(999).toList == List(1.1, 3.3, 5.5)
eachNElement(1.1 #:: 2.2 #:: 3.3 #:: 4.4 #:: 5.5 #:: 0.0 #:: 0.0 #:: LazyList(), 3, 9).take(999).toList == List(1.1, 4.4, 0.0)


//zadanie 5 (5pkt)
def ldzialanie(llist1: LazyList[Double], llist2: LazyList[Double], op: (Double, Double) => Double): LazyList[Double] ={
  (llist1, llist2) match{
    case (l1, LazyList()) => l1
    case (LazyList(), l2) => l2
    case (l1, l2) => op(l1.head,l2.head) #:: ldzialanie(l1.tail, l2.tail, op)
  }
}

ldzialanie(1#::LazyList(), 2#::LazyList(), _/_).take(1).toList == List(0.5)
ldzialanie(1.2#::LazyList(), 10.0#::20.0#::30.0#::40.0#::LazyList(), _*_).take(10).toList == List(12.0, 20.0, 30.0, 40.0)
ldzialanie(1.0#::LazyList(), 2.0#::2.0#::LazyList(), _/_).take(10).toList == List(0.5, 2.0)
ldzialanie(LazyList(), 10.0#::20.0#::30.0#::40.0#::LazyList(), _/_).take(10).toList == List(10.0, 20.0, 30.0, 40.0)
ldzialanie(10.0#::20.0#::30.0#::40.0#::LazyList(), LazyList(), _/_).take(10).toList == List(10.0, 20.0, 30.0, 40.0)
ldzialanie(0.0#::0.1#::LazyList(), (-1.0)#::(-0.9)#::LazyList(), _-_).take(10).toList == List(1.0, 1.0)
ldzialanie(0.0#::0.1#::LazyList(), (-1.0)#::(-0.9)#::LazyList(), _+_).take(10).toList == List(-1.0, -0.8)
ldzialanie(LazyList(), LazyList(), _+_).take(100).toList == List()
ldzialanie(LazyList(), LazyList(), _*_).take(100).toList == List()
ldzialanie(LazyList(), LazyList(), _/_).take(100).toList == List()
ldzialanie(LazyList(), LazyList(), _-_).take(100).toList == List()
