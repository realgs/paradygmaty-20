import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Random

object L6_tree {
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  //Not Parallel
  def treeDepth[A](tree: BT[A]): Int ={
    def checkTreeDepthIter(subtree: BT[A])(k: Int): Int  =
      subtree match{
        case Empty => k
        case Node(_ , left, right) => checkTreeDepthIter(left)(k+1)&checkTreeDepthIter(right)(k+1)
      }
    checkTreeDepthIter(tree)(0)
  }
  def isTreeDepthN[A](tree: BT[A])(n: Int): Boolean = treeDepth(tree) == n

  //Parallel
  def treeDepthPar[A](tree: BT[A]): Int ={
    def checkTreeDepthIter(subtree: BT[A])(k: Int): Int  =
      subtree match{
        case Empty => k
        case Node(_ , left, right) => {
          val leftFut = Future{checkTreeDepthIter(left)(k+1)}
          val rightFut = Future{checkTreeDepthIter(right)(k+1)}
          Await.result(leftFut, Duration.Inf)&Await.result(rightFut, Duration.Inf)
        }
      }
    checkTreeDepthIter(tree)(0)
  }
  def isTreeDepthNPar[A](tree: BT[A])(n: Int): Boolean = treeDepthPar(tree) == n

  //help Methods
  def generateTree(n: Int)(minValue: Int)(maxValue: Int): BT[Int] = {
    val r = new Random()
    def genSubTree(n: Int): BT[Int] =
      n match {
        case 0 => Empty
        case _ => Node(minValue + r.nextInt(maxValue - minValue + 1), genSubTree(n - 1), genSubTree(n - 1))
      }
    genSubTree(n)
  }

  def getNotFullTree(n: Int)(minValue: Int)(maxValue: Int): BT[Int] ={
    val r = new Random()
    def genSubTree(n: Int): BT[Int] =
      n match {
        case 0 => Empty
        case _ => Node(minValue + r.nextInt(maxValue - minValue + 1), genSubTree(n - 1), Empty)
      }
    genSubTree(n)
  }

  //Not Parallel
  def isTreeFull[A](tree: BT[A]): Boolean ={
    def checkSubTree(subtree: BT[A]): Boolean  =
      subtree match {
        case Empty => true
        case Node(_ , Empty, Empty) => true
        case Node(_ , Empty, _ ) => false
        case Node(_ , _ , Empty) => false
        case Node(_ , left, right) => (treeDepth(left) == treeDepth(right))&checkSubTree(left)&checkSubTree(right)
      }
    checkSubTree(tree)
  }

  //Parallel
  def isTreeFullPar[A](tree: BT[A]): Boolean ={
    def checkSubTreePar(subtree: BT[A]): Boolean  =
      subtree match {
        case Empty => true
        case Node(_ , Empty, Empty) => true
        case Node(_ , Empty, _ ) => false
        case Node(_ , _ , Empty) => false
        case Node(_ , left, right) => {
          val leftFut = Future{checkSubTreePar(left)}
          val rightFut = Future{checkSubTreePar(right)}
          (treeDepth(left) == treeDepth(right))&Await.result(leftFut, Duration.Inf)&Await.result(rightFut, Duration.Inf)
        }
      }
    checkSubTreePar(tree)
  }
}




