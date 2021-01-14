package Tests
import main.Fibonacci._
object FibonacciTest {

  def runTest(): Unit = {
    test(10,55)
    test(40, 102334155)
  }

  def test(fibonacciNum: Int, desirableNum: BigInt){
    println("Fibonacci test")
    println(fibonacci(fibonacciNum) == desirableNum)
    println(fibonacciFuture(fibonacciNum) == desirableNum)
    println(fibonacciParallel(fibonacciNum) == desirableNum)
  }

}
