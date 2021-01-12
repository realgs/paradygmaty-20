// Adrian Chłopowiec
package algorithms

import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.mutable.ParArray
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

sealed trait Tree[+A]
case object Empty extends Tree[Nothing]
case class Node[A](elem: A, var children: Array[Tree[A]]) extends Tree[A]

object TreeNodes
{
  def generateTree[Integer](depth: Int, childrenNum: scala.Int, valBound: scala.Int): Node[scala.Int] =
    {
      val random = scala.util.Random
      def generate(tree: Node[Int], depth: Int): Unit =
        {
          if(depth > 0)
          {
            tree.children = tree.children.map(_ => Node(random.nextInt(valBound), Array.fill[Tree[Int]](childrenNum)(Empty)))
            tree.children.foreach(child => generate(child.asInstanceOf[Node[Int]], depth - 1))
          }
        }

      val root = Node[Int](random.nextInt(valBound).asInstanceOf[java.lang.Integer], Array.fill[Tree[Int]](childrenNum)(Empty))
      generate(root, depth)
      root
    }

  def calculcateNodesValues(tree: Node[Int]): Long =
    {
      def calculate(tree: Tree[Int]): Long =
        {
          var acc: Long = 0
          tree match
            {
            case Empty => 0
            case Node(elem, children) =>
              children.foreach(child => acc += calculate(child))
              acc += elem
              acc
          }
        }
      var result: Long = tree.elem
      tree.children.foreach(child => result += calculate(child))
      result
    }
}

object TreeNodesPar
{
  def calculateNodesValues(tree: Node[Int], childrenNum: Int): Long =
    {
      calculateParallel(tree, parallelDepth(childrenNum))
    }

  private def calculateParallel(tree: Node[Int], parallelDepth: Int): Long =
    {
      def calculateSeq(tree: Tree[Int]): Long =
      {
        var acc: Long = 0
        tree match
        {
          case Empty => 0
          case Node(elem, children) =>
            children.foreach(child => acc += calculateSeq(child))
            acc += elem
            acc
        }
      }
      def calculatePar(tree: Tree[Int], parDepth: Int): Long =
      {
        var acc: Long = 0
        var results: ParArray[Long] = null
        tree match
        {
          case Empty => 0
          case Node(elem, children) =>
            if(parDepth > 0)
              results = children.par.map(child => calculatePar(child, parDepth - 1))
            else
              children.foreach(child => acc += calculateSeq(child))
            if(results != null)
              acc += results.sum
            acc += elem
            acc
        }
      }
      var result: Long = tree.elem
      var results: ParArray[Long] = null
      if(parallelDepth > 0)
        results = tree.children.par.map(child => calculatePar(child, parallelDepth - 1))
      else
        tree.children.foreach(child => result += calculateSeq(child))
      result += results.sum
      result
    }

  // Code for comparison with Futures and Await instead of parallel collection
  /*
  private def calculateParallel(tree: Node[Int], parallelDepth: Int): Long =
  {
    def calculateSeq(tree: Tree[Int]): Long =
    {
      var acc: Long = 0
      tree match
      {
        case Empty => 0
        case Node(elem, children) =>
          children.foreach(child => acc += calculateSeq(child))
          acc += elem
          acc
      }
    }
    def calculatePar(tree: Tree[Int], parDepth: Int): Long =
    {
      var acc: Long = 0
      var futures: Array[Future[Long]] = null
      tree match
      {
        case Empty => 0
        case Node(elem, children) =>
          if(parDepth > 0)
            futures = children.map(child => Future{calculatePar(child, parallelDepth - 1)})
          else
            children.foreach(child => acc += calculateSeq(child))
          if(futures != null)
            {
              val results = futures.map(f => Await.result(f, 600.seconds))
              acc += results.sum
            }
          acc += elem
          acc
      }
    }
    var result: Long = tree.elem
    var future: Array[Future[Long]] = null
    if(parallelDepth > 0)
      future = tree.children.map(child => Future{calculatePar(child, parallelDepth - 1)})
    else
      tree.children.foreach(child => result += calculateSeq(child))
    val results = future.map(f => Await.result(f, 600.seconds))
    result += results.sum
    result
  }*/

  private def parallelDepth(childrenNum: Int): Int =
    {
      val processors = Runtime.getRuntime.availableProcessors()
      var depth = 0
      var proc = 1
      while(proc * childrenNum <= processors)
        proc *= childrenNum
        depth += 1
      depth
    }
}
