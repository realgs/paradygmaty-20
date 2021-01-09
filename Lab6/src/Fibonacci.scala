import ParallelMechanism.parallel

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object Fibonacci {

  def fibonacci(num: Int): BigInt = {
    if (num < 0) throw new Exception(s"negative argument $num")
    else if (num == 0) 0
    else if (num == 1) 1
    else fibonacci(num - 1) + fibonacci(num - 2)
  }

  def fibonacciFuture(num: Int): BigInt = {
    if (num < 0) throw new Exception(s"negative argument $num")
    else if (num == 0) 0
    else if (num == 1) 1
    else {
      val f1 = Future(fibonacci(num - 1))
      val f2 = Future(fibonacci(num - 2))
      Await.result(f1, 1000.seconds) + Await.result(f2, 1000.seconds)
    }
  }

  def fibonacciParallel(num: Int): BigInt = {
    if (num < 0) throw new Exception(s"negative argument $num")
    else if (num == 0) 0
    else if (num == 1) 1
    else {
      val (result1, result2) = parallel(fibonacci(num - 1), fibonacci(num - 2))
      result1 + result2
    }
  }
}
