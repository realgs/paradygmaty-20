import org.scalatest.FunSuite

class LazyListsTest extends FunSuite{

  //task 4 tests
  test("eachNElement.empty"){
    assert(LazyLists.eachNElement(LazyList(), 2, 3) === LazyList())
  }
  test("eachNElement.error"){
    assertThrows[Exception](LazyLists.eachNElement(LazyList(5, 6, 3, 2, 1), -2, 3))
  }
  test("eachNElement.given_1"){
    assert(LazyLists.eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3) === LazyList(5, 3))
  }
  test("eachNElement.given_2"){
    assert(LazyLists.eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4) === LazyList(5, 3))
  }
  test("eachNElement.m_outside_size"){
    assert(LazyLists.eachNElement(LazyList(5, 6, 3, 2, 1), 2, 10) === LazyList(5, 3, 1))
  }
  test("eachNElement.n_outside_size"){
    assert(LazyLists.eachNElement(LazyList(5, 6, 3, 2, 1), 10, 4) === LazyList(5))
  }
  test("eachNElement.both_outside_size"){
    assert(LazyLists.eachNElement(LazyList(5, 6, 3, 2, 1), 10, 10) === LazyList(5))
  }
  test("eachNElement.LazyList.from(100)"){
    assert(LazyLists.eachNElement(LazyList.from(100), 20, 101) === LazyList(100, 120, 140, 160, 180, 200))
  }

  //task 5 tests
  test("ldzialanie.bothEmpty"){
    assert(LazyLists.ldzialanie(LazyList(), LazyList(), LazyLists.+) === LazyList())
  }
  test("ldzialanie.1stEmpty"){
    assert(LazyLists.ldzialanie(LazyList(), LazyList(1, 2, 3), LazyLists.+) === LazyList(1, 2, 3))
  }
  test("ldzialanie.2ndEmpty"){
    assert(LazyLists.ldzialanie(LazyList(2, 3, 5), LazyList(), LazyLists.+) === LazyList(2, 3, 5))
  }
  test("ldzialanie.+"){
    assert(LazyLists.ldzialanie(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), LazyLists.+) === LazyList(3, 5, 7, 5))
  }
  test("ldzialanie.-"){
    assert(LazyLists.ldzialanie(LazyList(1.1, 2, 3.5), LazyList(2.1, 3, 4.5, 5), LazyLists.-) === LazyList(-1, -1, -1, 5))
  }
  test("ldzialanie.*"){
    assert(LazyLists.ldzialanie(LazyList(1, 2.5, 3.5, 9, 6.7), LazyList(2.1, 3, -4.5, 0), LazyLists.*) === LazyList(2.1, 7.5, -15.75, 0, 6.7))
  }
  test("ldzialanie./"){
    assert(LazyLists.ldzialanie(LazyList(1.1, 2, 22, 1, 12, 10), LazyList(2, 4, 11, 5, -4, 2.5, 0), LazyLists./) === LazyList(0.55, 0.5, 2, 0.2, -3, 4, 0))
  }
  test("ldzialanie./_with0"){
    assertThrows[Exception](LazyLists.ldzialanie(LazyList(1.1, 2, 3.6), LazyList(2, 0, 2, 5), LazyLists./).force)
  }

}
