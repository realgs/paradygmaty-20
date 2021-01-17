import Tests.{FibonacciNumTest, PrimeNumbersTest, QuicksortTest, SameTreesTest, Test}

object Main extends App {

  QuicksortTest.arrayTest()
  FibonacciNumTest.fibTest()
  SameTreesTest.areTreesTheSameTest()
  PrimeNumbersTest.primesTest()
  Test.runTests()
}
