import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Fibonacci {
  def findFibonacci(n: Int): BigInt = {
    n match {
      case 0 => 0
      case 1 => 1
      case _ => findFibonacci(n - 1) + findFibonacci(n - 2)
    }
  }

  def findFibonacciIter(n: Int): BigInt = {
    def fibInner(n: Int, f1: Int, f2: Int): BigInt = {
      n match {
        case 0 => f1
        case 1 => f2
        case _ => fibInner(n - 1, f2, f1 + f2)
      }
    }

    if (n > 0) fibInner(n, 0, 1)
    else throw new Exception("n must be positive")
  }

  def findFibonacciParallel(n: Int): BigInt = {
    n match {
      case 0 => 0
      case 1 => 1
      case _ => {
        val f1 = Future(findFibonacci(n - 1))
        val f2 = Future(findFibonacci(n - 2))
        Await.result(f1, Duration.Inf) + Await.result(f2, Duration.Inf)
      }
    }
  }
}
