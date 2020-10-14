import org.scalatest.FunSuite

class Lista2Test extends FunSuite {
  test("Lista2Test.mult") {
    assert(Lista2.mult(List(1,2,3,4)) == 24)
    assert(Lista2.mult(List(4, -5, 2, 100, 2)) == -8000)
    assert(Lista2.mult(List(100,200,300)) == 6000000)
    assert(Lista2.mult(List()) == 0)
  }

  test("Lista2.connectString") {
    assert(Lista2.connectString(List("I", "like", "programming")," ", "!") == "I like programming!" )
    assert(Lista2.connectString(List("one", "two", "three"),"*", ".") == "one*two*three." )
    assert(Lista2.connectString(List()," ", "?") == " " )
  }

  test("Lista2Tets.interval") {
    assert(Lista2.interval(List(1, 2, 3, 4), 1, 5) == true)
    assert(Lista2.interval(List(1, 2.6, 32.8, 400.432), 1, 445.97) == true)
    assert(Lista2.interval(List(-100, -20.8, 320, -4.6, 15), -50, 320) == false)
    assert(Lista2.interval(List(), 1, 5) == true)
  }

  test("Lista2.pow") {
    assert(Lista2.pow(2, 3) == 8)
    assert(Lista2.pow(-3, 3) == -27)
    assert(Lista2.pow(20, 0) == 1)
    assertThrows[Exception] {
      Lista2.pow(10, -5)
    }
  }
}
