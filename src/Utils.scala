import Functions.{Empty, Node, BT}

object Utils {
  def height[A] (tree: BT[A]): Int =
    tree match {
      case Empty => 0
      case Node(_, Empty, Empty) => 0
      case Node(_, leftSubtree, rightSubtree) => 1 + Math.max(height(leftSubtree), height(rightSubtree))
    }

  def isTreeFull[A] (tree: BT[A]): Boolean = {
    def isTreeFullHelper(t: BT[A]): Boolean =
      t match {
        case Empty => true
        case Node(_ , leftSubtree, Empty) => if(leftSubtree == Empty) true else false
        case Node(_ , Empty, rightSubtree) => if(rightSubtree == Empty) true else false
        case Node(_ , leftSubtree, rightSubtree) => isTreeFullHelper(leftSubtree) && isTreeFullHelper(rightSubtree)
      }
    isTreeFullHelper(tree)
  }

  val fib: LazyList[Double] = {
    def fibHelper(a: Double, b: Double): LazyList[Double] =
      a #:: fibHelper(b, a + b)
    fibHelper(0, 1)
  }

  val + : (Double, Double) => Double = (a, b) => a + b
  val - : (Double, Double) => Double = (a, b) => a - b
  val * : (Double, Double) => Double = (a, b) => a * b
  val / : (Double, Double) => Double = (a, b) => a / b

  val add : (Int, Int) => Int = (a, b) => a + b
  val sub : (Int, Int) => Int = (a, b) => a - b
  val mul : (Int, Int) => Int = (a, b) => a * b
  val div : (Int, Int) => Int = (a, b) => a / b

  val ^ : (String, String) => String = (a, b) => a + b
}
