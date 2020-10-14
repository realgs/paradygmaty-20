package List2Test
import org.scalatest.FunSuite
import List2.L2

class L2Test extends FunSuite {
  test("Test of 'product' function"){
    assert(L2.product(List(3,1,5,2)) == 30)
    assert(L2.product(List()) == 0)
    assert(L2.product(List(-3,-1,-1,1,3,2)) == -18)
    assert(L2.product(List(-2)) == -2)
    assert(L2.product(List(1.5,4)) == 6)
    assert(L2.product(List(1.5,2.5,-0.25)) == -0.9375)
  }

  test("Test of 'makeSentence' function"){
    assert(L2.makeSentence(List("Testing", "makeSentence", "function"), ';', '.') == "Testing;makeSentence;function.")
    assert(L2.makeSentence(List(), ';', '!') == "")
    assert(L2.makeSentence(List("Hello"), '_', '!') == "Hello!")
    assert(L2.makeSentence(List("1","2","3"), '&', '!') == "1&2&3!")
    assert(L2.makeSentence(List("Have", "a", "nice", "day"), ' ', '!') == "Have a nice day!")
  }

  test("Test of 'isInRange' function"){
    assert(L2.isInRange(List(1,2,3), -1, 3) == true)
    assert(L2.isInRange(List(), -1, 0) == true)
    assert(L2.isInRange(List(5), 5, 5) == true)
    assert(L2.isInRange(List(2.5, 6.7, 10.2), 1.5, 19) == true)
    assert(L2.isInRange(List(-2.5, 6, 11), 1.5, 15.5) == false)
    val thrownException = intercept[Exception] {
      L2.isInRange(List(2,3), 8, 5)
    }
    assert(thrownException.getMessage == "Incorrect range")
  }

  test("Test of 'power' function"){
    assert(L2.power(4,2) == 16)
    assert(L2.power(2.5, 3) == 15.625)
    assert(L2.power(0, -30) == 0)
    assert(L2.power(3,0) == 1)
    assert(L2.power(-4.6, 6) == 9474.296895999996)
    assert(L2.power(2, -3) == 0.125)
  }
}

