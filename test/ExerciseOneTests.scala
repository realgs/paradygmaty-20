import org.scalatest.flatspec.AnyFlatSpec
import functions.ExerciseOne._

class ExerciseOneTests extends AnyFlatSpec {
  "Exercise 1 function" should "split given list into two lists, first with negative numbers, second with negative and also odd numbers" in {
    assert(createTwoLists(List(-3, -6, 8, -9, 13)) == (List(-3, -6, -9), List(-3, -9)))
    assert(createTwoLists(List(-2, -3, 15, -8, 0, -3)) == (List(-2, -3, -8, -3), List(-3, -3)))
  }

  "Exercise 1 function" should "return tuple of Nils for list of only positive numbers" in {
    assert(createTwoLists(List(3, 1, 2, 6, 2, 13)) == (Nil, Nil))
  }

  "Exercise 1 function" should "return tuple of Nils for empty list" in {
    assert(createTwoLists(List(-3, -6, 8, -9, 13)) == (List(-3, -6, -9), List(-3, -9)))
    assert(createTwoLists(List(-2, -3, 15, -8, 0, -3)) == (List(-2, -3, -8, -3), List(-3, -3)))
  }

}
