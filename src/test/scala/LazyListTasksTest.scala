import fourthFifthTasks.{LazyListTasks, Operators}
import org.scalatest.FunSuite

class LazyListTasksTest extends FunSuite{
  test("eachNElemen function test:") {
    assert(LazyListTasks.eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3) == LazyList(5, 3))
    assert(LazyListTasks.eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4) == LazyList(5, 3))
    assert(LazyListTasks.eachNElement(LazyList(477, 88, 99, 100), 1, 1) == LazyList(477))
    assert(LazyListTasks.eachNElement(LazyList(-100, 400, -234, 546, 23, -98, 19, 22, 222), 1, 8) == LazyList(-100, 400, -234, 546, 23, -98, 19, 22))
    assert(LazyListTasks.eachNElement(LazyList(12, 43, 2, 8, 43, 21, 9, 5, 100), 3, 5) == LazyList(12, 8))
    assert(LazyListTasks.eachNElement(LazyList(), 4, 9) == LazyList())
    assert(LazyListTasks.eachNElement(LazyList("ala", "ma", "kota", "psa", "i", "rybki"), 2, 4) == LazyList("ala", "kota"))
    assert(LazyListTasks.eachNElement(LazyList("ala", "ma", "kota", "psa", "i", "rybki"), 2, 5) == LazyList("ala", "kota", "i"))
    assertThrows[Exception](LazyListTasks.eachNElement(LazyList(1, 2, 3, 4, 4, 6, 7, 8, 8, 9), 0, 7))
    assertThrows[Exception](LazyListTasks.eachNElement(LazyList(1, 2, 3, 4, 4, 6, 7, 8, 8, 9), 6, 0))
    assertThrows[Exception](LazyListTasks.eachNElement(LazyList(2.3, 6.9, 100, 543, 999, 888), -3, 5))
    assertThrows[Exception](LazyListTasks.eachNElement(LazyList(2.3, 6.9, 100, 543, 999, 888), 3, -5))
  }

  test("ldzialanie function test:") {
    assert(LazyListTasks.ldzialanie(LazyList(5,6,3,2,1),LazyList(4,5,8,10,-1,8,0,1 ), Operators.+) == LazyList(9, 11,11,12,0,8,0,1))
    assert((LazyListTasks.ldzialanie(LazyList(9, 8, 7, 6, 5), LazyList(1,2,3,4,5), Operators.-))==LazyList(8, 6, 4, 2, 0))
    assert((LazyListTasks.ldzialanie(LazyList(9, 8, 7, 6, 5), LazyList(1,2,3,4,5,6,9,2), Operators.*))==LazyList(9, 16, 21, 24, 25, 6, 9, 2))
    assert(LazyListTasks.ldzialanie(LazyList(100,200,300,400,500), LazyList(10, 20, 30, 40,50), Operators./)== LazyList(10,10,10,10,10))
    assertThrows[Exception](LazyListTasks.ldzialanie(LazyList(100,200,300,400,500), LazyList(10, 20, 0, 40,50), Operators./)==LazyList(10,10))
  }
}

