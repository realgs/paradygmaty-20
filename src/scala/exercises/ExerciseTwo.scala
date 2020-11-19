package scala.exercises

import scala.helper.HelperFunctions.depth
import scala.trees._

object ExerciseTwo {
  def createSubtractionTree(firstTree: BT[Int], secondTree: BT[Int]): BT[Int] = {
    def helper(fstTree: BT[Int], sndTree: BT[Int]): BT[Int] = {
      (fstTree, sndTree) match {
        case (Empty, Empty) => Empty
        case (Node(fstValue, fstLeft, fstRight), Node(sndValue, sndLeft, sndRight)) => Node(fstValue - sndValue, helper(fstLeft, sndLeft), helper(fstRight, sndRight))
      }
    }
    if (depth(firstTree) == depth(secondTree)) helper(firstTree, secondTree) else throw new IllegalArgumentException
  }
}
