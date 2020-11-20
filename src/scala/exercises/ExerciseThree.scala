package scala.exercises
import trees._
import helper.HelperFunctions._

object ExerciseThree {
  //Zad3 depth(1pkt)
  def removeDuplicatesDepth(firstTree: BT[Int], secondTree: BT[Int]): (BT[Int], BT[Int]) = {
    def helper(fstTree: BT[Int], sndTree: BT[Int]): (BT[Int], BT[Int]) = {
      (fstTree, sndTree) match {
        case (Empty, Empty) => (Empty, Empty)
        case (Node(fstValue, fstLeft, fstRight), Node(sndValue, sndLeft, sndRight)) =>
          if (fstValue != sndValue) {
            val leftTrees = helper(fstLeft, sndLeft)
            val rightTrees = helper(fstRight, sndRight)
            (Node(fstValue, leftTrees._1, rightTrees._1), Node(sndValue, leftTrees._2, rightTrees._2))
          }
          else {
            if (fstLeft != sndLeft || fstRight != sndRight) (Node(-1, fstLeft, fstRight), Node(-1, sndLeft, sndRight))
            else (Empty, Empty)
          }
      }
    }
    if (areTreesValidForThisTask(firstTree, secondTree)) helper(firstTree, secondTree) else throw new IllegalArgumentException
  }
}
