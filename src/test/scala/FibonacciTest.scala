import algorithms.Fibonacci
import org.scalatest.FunSuite

class FibonacciTest extends FunSuite{

  test("sequentialFibonacciTest") {
    assert(Fibonacci.sequentially(0) == 0)
    assert(Fibonacci.sequentially(1) == 1)
    assert(Fibonacci.sequentially(10) == 55)
    assert(Fibonacci.sequentially(20) == 6765 )
  }

  test("parallelFibonacciTest") {
    assert(Fibonacci.parallel(0) == 0)
    assert(Fibonacci.parallel(1) == 1)
    assert(Fibonacci.parallel(10) == 55)
    assert(Fibonacci.parallel(20) == 6765 )
  }
}
