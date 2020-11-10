package Lista4
import Lista4.Lista4LazyLists.{eachNElement, lDzialanie}
import org.scalatest.FunSuite

class Lista4LazyListsTest extends FunSuite {

  test("Lista 4 zadanie 4") {
    assert(eachNElement(LazyList(5,6,3,2,1), 2, 4) == LazyList(5,3))
    assert(eachNElement(LazyList(5,6,3,2,1), 2, 3) == LazyList(5,3))
    assert(eachNElement(LazyList.from(14), 3, 13) == LazyList(14, 17, 20, 23, 26))
    assert(eachNElement(LazyList(5,6,3,2,1), 2, 20) == LazyList(5,3,1)) // test na za duze m
    assert(eachNElement(LazyList(5,6,3,2,1), 40, 4) == LazyList(5)) // test na za duze n
    assert(eachNElement(LazyList(5,6,3,2,1), 40, 20) == LazyList(5)) // test na za duze n i m
  }

  test("Lista 4 zadanie 5") {
    assert(lDzialanie(LazyList(1,2,3), LazyList(2,3,4,5), Lista4LazyLists.addition) == LazyList(3,5,7,5))
    assert(lDzialanie(LazyList(12,11.5,4,2,4.4), LazyList(2,3,4,5), Lista4LazyLists.subtraction) == LazyList(10,8.5, 0,-3,4.4))
    assert(lDzialanie(LazyList(11,2,3,4.5), LazyList(2.5,3,4,5,2), Lista4LazyLists.multiplication) == LazyList(27.5,6,12,22.5,2))
    assert(lDzialanie(LazyList(30, 40.5, 8, 1), LazyList(3, 2, 16, 10, 44.4), Lista4LazyLists.division) == LazyList(10, 20.25, 0.5, 0.1, 44.4))
    assertThrows[ArithmeticException] {
      lDzialanie(LazyList(1,2,3), LazyList(0,3,4,5), Lista4LazyLists.division).force
    }
  }

}
