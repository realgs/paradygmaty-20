import scala.util.Random
import scala.collection.parallel.CollectionConverters._

object Trees {
  class Tree(val children: List[Tree] = Nil, var value: Int = 0) { }

  def generateRegularTree(depth: Int, children: Int): Tree = {
    def generate(depth: Int): Tree = {
      if(depth > 0) {
        new Tree(List.fill(children)(generate(depth-1)))
      } else {
        new Tree(Nil, Random.nextInt())
      }
    }

    if (depth < 0) throw new IllegalArgumentException("depth cannot be negative")
    if (children < 0) throw new IllegalArgumentException("children cannot be negative")
    if (children == 0) generate(0)
    else generate(depth)
  }

  def treeLeavesMin(tree: Tree): Int = {
    tree.children match {
      case _ :: _ => {
        tree.children.foreach(child => treeLeavesMin(child))
        tree.value = tree.children.map(child => child.value).min
      }
      case _ => ()
    }
    tree.value
  }

  def treeLeavesMinParallel(tree: Tree): Int = {
    tree.children match {
      case _ :: _ => {
        tree.children.par.foreach(child => treeLeavesMin(child))
        tree.value = tree.children.map(child => child.value).min
      }
      case _ => ()
    }
    tree.value
  }
}
