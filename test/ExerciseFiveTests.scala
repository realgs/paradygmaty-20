import org.scalatest.flatspec.AnyFlatSpec
import functions.ExerciseFive._

class ExerciseFiveTests extends AnyFlatSpec{
  "Exercise 5 function" should "concatenate three lists" in {
    assert(joinLists(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
    assert(joinLists(List("la", "la"), List("ba", "ba"), List("wo", "mo", "ro")) == List("la", "la", "ba", "ba", "wo", "mo", "ro"))
  }

  "Exercise 5 function" should "work properly when some lists are Nil" in {
    assert(joinLists(Nil, List(1, 0), List(9)) == List(1, 0, 9))
    assert(joinLists(List(5, 4, 3, 2), Nil, List(9)) == List(5, 4, 3, 2, 9))
    assert(joinLists(List(5, 4, 3, 2), List(1, 0), Nil) == List(5, 4, 3, 2, 1, 0))
    assert(joinLists(Nil, Nil, List(9)) == List(9))
    assert(joinLists(List(5, 4, 3, 2), Nil, Nil) == List(5, 4, 3, 2))
    assert(joinLists(Nil, List(1, 0), List(9)) == List(1, 0, 9))
    assert(joinLists(Nil, Nil, Nil) == Nil)
  }

  "Exercise 5 function with tail recursion" should "concatenate three lists" in {
    assert(joinListsWithTailRecursion(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
    assert(joinListsWithTailRecursion(List("la", "la"), List("ba", "ba"), List("wo", "mo", "ro")) == List("la", "la", "ba", "ba", "wo", "mo", "ro"))
  }

  "Exercise 5 function with tail recursion" should "work properly when some lists are Nil" in {
    assert(joinListsWithTailRecursion(Nil, List(1, 0), List(9)) == List(1, 0, 9))
    assert(joinListsWithTailRecursion(List(5, 4, 3, 2), Nil, List(9)) == List(5, 4, 3, 2, 9))
    assert(joinListsWithTailRecursion(List(5, 4, 3, 2), List(1, 0), Nil) == List(5, 4, 3, 2, 1, 0))
    assert(joinListsWithTailRecursion(Nil, Nil, List(9)) == List(9))
    assert(joinListsWithTailRecursion(List(5, 4, 3, 2), Nil, Nil) == List(5, 4, 3, 2))
    assert(joinListsWithTailRecursion(Nil, List(1, 0), List(9)) == List(1, 0, 9))
    assert(joinListsWithTailRecursion(Nil, Nil, Nil) == Nil)
  }
}
