import org.scalatest.flatspec.AnyFlatSpec
import functions.ExerciseThree._

class ExerciseThreeTests extends AnyFlatSpec {
  "Exercise 3 function" should "correctly combine two lists into one" in {
    assert(combineLists(List(-1, 2, 3), List(0, 1, 2)) == List(-1, 0, 2, 1, 3, 2))
    assert(combineLists(List("ala ", "duzego "), List("ma ", "psa")) == List("ala ", "ma ", "duzego ", "psa"))
    assert(combineLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6))
  }

  "Exercise 3 function" should "return second list if first is Nil" in {
    assert(combineLists(Nil, List(0, 1, 2)) == List(0, 1, 2))
  }

  "Exercise 3 function" should "return first list if second is Nil" in {
    assert(combineLists(List(-1000, 4, 22), Nil) == List(-1000, 4, 22))
  }

  "Exercise 3 function" should "return Nil if both lists are Nil" in {
    assert(combineLists(Nil, Nil) == Nil)
  }

}
