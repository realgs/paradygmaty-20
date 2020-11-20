import org.scalatest.flatspec.AnyFlatSpec

import scala.exercises.ExerciseOne.generateTree
import scala.exercises.ExerciseThree._
import scala.trees._

class ExerciseThreeTests extends AnyFlatSpec {
  "Exercise 3 function" should "correctly delete from two trees" in {
    val firstTree = Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(3, Node(6, Empty, Empty), Node(7, Empty, Empty)))
    val secondTree = Node(2, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(3, Node(8, Empty, Empty), Node(8, Empty, Empty)))
    assert(removeDuplicatesDepth(firstTree, secondTree) == (Node(1, Empty, Node(-1, Node(6, Empty, Empty), Node(7, Empty, Empty))), Node(2, Empty, Node(-1, Node(8, Empty, Empty), Node(8, Empty, Empty)))))

    val firstTree2 = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))
    val secondTree2 = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))
    assert(removeDuplicatesDepth(firstTree2, secondTree2) == (Empty, Empty))

    val firstTree3 = Node(5, Node(2, Empty, Empty), Node(4, Empty, Empty))
    val secondTree3 = Node(5, Node(2, Empty, Empty), Node(3, Empty, Empty))
    assert(removeDuplicatesDepth(firstTree3, secondTree3) == (Node(-1, Node(2, Empty, Empty), Node(4, Empty, Empty)), Node(-1, Node(2, Empty, Empty), Node(3, Empty, Empty))))

    val firstTree4 = Node(1, Node(2, Node(4, Node(8, Empty, Empty), Node(9, Empty, Empty)), Node(5, Node(10, Empty, Empty), Node(11, Empty, Empty))), Node(3, Node(6, Node(12, Empty, Empty), Node(13, Empty, Empty)), Node(7, Node(14, Empty, Empty), Node(15, Empty, Empty))))
    val secondTree4 = Node(0, Node(1, Node(3, Node(7, Empty, Empty), Node(8, Empty, Empty)), Node(4, Node(9, Empty, Empty), Node(10, Empty, Empty))), Node(2, Node(6, Node(11, Empty, Empty), Node(12, Empty, Empty)), Node(7, Node(14, Empty, Empty), Node(15, Empty, Empty))))
    assert(removeDuplicatesDepth(firstTree4, secondTree4) ==
      (Node(1,Node(2,Node(4,Node(8,Empty,Empty),Node(9,Empty,Empty)),Node(5,Node(10,Empty,Empty),Node(11,Empty,Empty))),Node(3,Node(-1,Node(12,Empty,Empty),Node(13,Empty,Empty)),Empty)),
        Node(0,Node(1,Node(3,Node(7,Empty,Empty),Node(8,Empty,Empty)),Node(4,Node(9,Empty,Empty),Node(10,Empty,Empty))),Node(2,Node(-1,Node(11,Empty,Empty),Node(12,Empty,Empty)),Empty))))
  }

  "Exercise 3 function" should "return tuple of Empty's for tuple of Empty trees" in {
    assert(removeDuplicatesDepth(Empty, Empty) == (Empty, Empty))
  }

  "Exercise 3 function" should "throw an IllegalArgumentException if trees' depths are different" in {
    assertThrows[IllegalArgumentException]{removeDuplicatesDepth(generateTree(3, 2, 5), generateTree(4, 2, 5))}
  }
}
