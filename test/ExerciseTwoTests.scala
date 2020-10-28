import org.scalatest.flatspec.AnyFlatSpec
import functions.ExerciseTwo._

class ExerciseTwoTests extends AnyFlatSpec {
  "Exercise 2 function" should "return correct size" in {
    assert(listSize(List(-2, -3, 0, 1000)) == 4)
    assert(listSize(List(List("ala"), List("ma kota"))) == 2)
  }

  "Exercise 2 function" should "return 0 for Nil" in {
    assert(listSize(Nil) == 0)
  }

}
