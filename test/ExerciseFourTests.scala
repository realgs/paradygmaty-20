import org.scalatest.flatspec.AnyFlatSpec

import scala.exercises.ExerciseFour.eachNElement

class ExerciseFourTests extends AnyFlatSpec {
  "Exercise four function" should "generate lazy list with given requirements" in {
    val lazyList = LazyList(5, 6, 3, 2, 1)

    assert(eachNElement(lazyList, 2, 3).toList == List(5, 3))
    assert(eachNElement(lazyList, 2, 4).toList == List(5, 3))
    assert(eachNElement(lazyList, 2, 5).toList == List(5, 3, 1))
  }

  "Exercise four function" should "fail when n <= 0 or m < 0" in {
    val lazyList = LazyList(-5, 6, 0, 2, 1)

    assertThrows[IllegalArgumentException](eachNElement(lazyList, 0, 2))
    assertThrows[IllegalArgumentException](eachNElement(lazyList, 2, -1))
  }

  "Exercise four function" should "return empty lazy list when m == 0" in {
    val lazyList = LazyList(-5, 6, 0, 2, 1)

    assert(eachNElement(lazyList, 2, 0) == LazyList())
  }
}
