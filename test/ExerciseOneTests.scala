import org.scalatest.flatspec.AnyFlatSpec

import scala.exercises.ExerciseOne._
import scala.helper.HelperFunctions._
import scala.trees._

class ExerciseOneTests extends AnyFlatSpec {

  "Exercise one function" should "give correct results for normal input" in {
    assert(treeTest(generateTree(20, 2, 5), 2, 5))
    assert(treeTest(generateTree(15, 5, 100), 5, 100))
  }

  "Exercise one function" should "return tree with given depth" in {
    assert(depth(generateTree(20, 2, 5)) == 20)
    assert(depth(generateTree(15, 10, 50)) == 15)
  }

  "Exercise one function" should "return Empty node for N <= 0" in {
    assert(generateTree(-2, 2, 5) == Empty)
    assert(generateTree(0, 1, 11) == Empty)
  }

  "Exercise one function" should "fail when start or end is <= 0" in {
    assertThrows[IllegalArgumentException](generateTree(3, -3, 2))
    assertThrows[IllegalArgumentException](generateTree(4, 2, -5))
    assertThrows[IllegalArgumentException](generateTree(3, 0, 0))
  }

  "Exercise one function" should "fail when start < end" in {
    assertThrows[IllegalArgumentException](generateTree(2, 5, 2))
  }

  "Exercise one function" should "return tree with the same elements when start == end" in {
    assert(treeTest(generateTree(5, 2, 2), 2, 2))
  }
}
