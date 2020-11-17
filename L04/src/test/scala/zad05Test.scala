import org.scalatest.FunSuite

class zad05Test extends FunSuite {
  test("zad05") {
    val addition = (a: Int, b: Int) => a + b
    val subtraction = (a: Int, b: Int) => a - b
    val multiplication = (a: Int, b: Int) => a * b
    val division = (a: Int, b: Int) => a / b

    assert(zad05.ldzialanie(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), addition) == LazyList(3, 5, 7, 5))
    assert(zad05.ldzialanie(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), subtraction) == LazyList(-1, -1, -1, 5))
    assert(zad05.ldzialanie(LazyList(2, 3, 4, 5), LazyList(2, 3, 4), multiplication) == LazyList(4, 9, 16, 5))
    assert(zad05.ldzialanie(LazyList(5, 10, 15, 20), LazyList(5, 2, 3, 7), division) == LazyList(1, 5, 5, 2))
    assert(zad05.ldzialanie(LazyList(), LazyList(1, 2), addition) == LazyList(1, 2))
    assert(zad05.ldzialanie(LazyList(1, 2), LazyList(), addition) == LazyList(1, 2))
    assert(zad05.ldzialanie(LazyList(), LazyList(), addition) == LazyList())
  }
}
