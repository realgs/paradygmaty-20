import org.scalatest.flatspec.AnyFlatSpec
import helper.HelperFunctions._
import exercises.ExerciseOne._
import exercises.ExerciseTwo._

class ExerciseTwoTests extends AnyFlatSpec {
  "Exercise two function" should "generate tree with node values containing first tree and second tree nodes subtraction" in {
    val firstTree = generateTree(3, 5, 5)
    val secondTree = generateTree(3, 2, 2)

    assert(treeTest(createSubtractionTree(firstTree, secondTree), 3, 3))
  }

  "Exercise two function" should "fail when depth's of arguments are different" in {
    val firstTree = generateTree(4, 5, 7)
    val secondTree = generateTree(3, 4, 7)

    assertThrows[IllegalArgumentException](createSubtractionTree(firstTree, secondTree))
  }
}
