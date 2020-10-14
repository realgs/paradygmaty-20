import org.scalatest.FunSuite
import List2.L2

class L2Test extends FunSuite {

  test("Test for 'product' function:") {
    assert(L2.product(List(4.5, 0, 6.5)) == 0)
    assert(L2.product(List(-5.5, 0.25, 5)) == -6.875)
    assert(L2.product(List(-5.5, 0.25, -5, 1)) == 6.875)
    assert(L2.product(List(-7.9)) == -7.9)
    assert(L2.product(List()) == 0)
  }

  test("Test for 'createString' function:") {
    assert(L2.createString(List("Ala","ma","kota"),',','!') == "Ala,ma,kota!")
    assert(L2.createString(List("Koniec"), ';', '?') == "Koniec?")
    assertThrows[Exception](L2.createString(List(),':','!'))
  }

  test("Test for 'checkRange' function: ") {
    assert(L2.checkRange(List(4, 5, 8, 0), 10, -5))
    assert(!L2.checkRange(List(3.5,0.3,-6.9,2.1), 0, 5))
    assert(!L2.checkRange(List(3,0,-6,2), -8, 0))
    assert(L2.checkRange(List(-4.2,-4.1,0.75,2.2), -5, 2.5))
    assert(L2.checkRange(List(),-5,6))
  }

  test("Test for 'countPower' function:") {
    assert(L2.countPower(2,5) == 32)
    assert(L2.countPower(2, -5) == 0.03125)
    assert(L2.countPower(2,0) == 1)
    assert(L2.countPower(-9,2) == 81)
    assert(L2.countPower(2.5, 2) == 6.25)
  }

}
