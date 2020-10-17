import org.scalatest.funsuite.AnyFunSuite

class FunctionsTest extends AnyFunSuite {
    test("doubleProduct") {
      assert(Functions.doubleProduct(List(5, 2, 9, 1, 2, 8, 5)) === 7200)
      assert(Functions.doubleProduct(List(6.24, 3.78, 1235.9, 45.87)) - 1337175.6574176 < 0.0001)
      assert(Functions.doubleProduct(List(-5.2, -6.7, -8.24)) - (-287.0816) < 0.0001)
      assert(Functions.doubleProduct(List(1.0, -1.0, 0.0)) === 0.0)
      assert(Functions.doubleProduct(Nil) === 0.0)
      assert(Functions.doubleProduct(null) === 0.0)

    }

    test("combineTokens") {
      assert(Functions.combineTokens(List("I","love","it"), "!", " ") === "I love it!")
    }

    test("listInRange") {
      assert(Functions.listInRange(List(1, 2, 3, 4), 0, 5) === true)
    }

    test("power") {
      assert(Functions.power(2, 5) === 32)
    }

}
