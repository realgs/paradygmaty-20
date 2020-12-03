import org.scalatest.funsuite.AnyFunSuite

class Task_1_2_Test extends AnyFunSuite {
  test("task1.test") {
    assert(Task_1_2.repeat(List(2, 4, 3, 1), List(1, 2, 3)) == List(2, 4, 4, 3, 3, 3))
    assert(Task_1_2.repeat(List("ala", "ma", "kota"), List(0, 2, 1, 3, 9, 4)) == List("ma", "ma","kota"))
    assert(Task_1_2.repeat(List('+', '-', '+', '=', '/','?', '*'), List(2, 0, 4, 1)) == List('+', '+', '+', '+', '+', '+', '='))
    assert(Task_1_2.repeat(List(), List(1, 2, 3)) == Nil)
    assert(Task_1_2.repeat(List(20, 40, 34, 1), List()) == Nil)
  }

  test("task2.test") {
    assert(Task_1_2.repeateWithoutDublicates(List(2,4,3,1), List(1,2,3)) == List(2, 4, 4, 3, 3, 3))
    assert(Task_1_2.repeateWithoutDublicates(List("ala", "ma", "kota"), List(0, 2, 1, 3, 9, 4)) == List("ma", "ma", "kota"))
    assert(Task_1_2.repeateWithoutDublicates(List(), List(1, 2, 3)) == Nil)
    assertThrows[Exception](Task_1_2.repeateWithoutDublicates(List('+', '-', '+', '=', '/','?', '*'), List(2, 0, 4, 1)))
    assertThrows[Exception](Task_1_2.repeateWithoutDublicates(List(10.3, 34,6, 10.3, 31.0), List(2, 3, 4)))
    assertThrows[Exception](Task_1_2.repeateWithoutDublicates(List(20, 40, 34, 20, 8), List()))
  }
}
