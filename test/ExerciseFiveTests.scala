import org.scalatest.flatspec.AnyFlatSpec

import exercises.ExerciseFive.ldzialanie

class ExerciseFiveTests extends AnyFlatSpec {
  "Exercise five function" should "return correct lazy list for basic arithmetic functions" in {
    val addition = (a: Int, b: Int) => a + b
    val subtraction = (a: Int, b: Int) => a - b
    val multiplication = (a: Int, b: Int) => a * b
    val division = (a: Int, b: Int) => a / b

    assert(ldzialanie(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), addition) == LazyList(3, 5, 7, 5))
    assert(ldzialanie(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), subtraction) == LazyList(-1, -1, -1, 5))
    assert(ldzialanie(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), multiplication) == LazyList(2, 6, 12, 5))
    assert(ldzialanie(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), division) == LazyList(1 / 2, 2 / 3, 3 / 4, 5))
  }

  "Exercise five function" should "throw an ArithmeticException if any of the arguments of the second list is 0 and function is division (when lazy list is evaluated)" in {
    val division = (a: Int, b: Int) => a / b

    assertThrows[ArithmeticException](ldzialanie(LazyList(1, 2, 3), LazyList(1, 0, 1), division).toList)
  }

  "Exercise five function" should "return first list if second is an empty list" in {
    val addition = (a: Int, b: Int) => a + b

    assert(ldzialanie(LazyList(-1, 0, 1), LazyList(), addition) == LazyList(-1, 0, 1))
  }

  "Exercise five function" should "return second list if first is an empty list" in {
    val addition = (a: Int, b: Int) => a + b

    assert(ldzialanie(LazyList(), LazyList(-1, 0, 1), addition) == LazyList(-1, 0, 1))
  }

  "Exercise five function" should "return empty list if both lists are an empty list" in {
    val addition = (a: Int, b: Int) => a + b

    assert(ldzialanie(LazyList(), LazyList(), addition) == LazyList())
  }
}
