import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt

object FibonacciNum {

  def fib(n: Int): Int = {
    if (n < 0) throw new IllegalArgumentException("cannot generate negative Fibonacci number")
    else {
      n match {
        case n if n < 2 => 1
        case _ => fib(n - 1) + fib(n - 2)
      }
    }
  }

  def fibParallel(n: Int): Int = {
    if (n < 0) throw new IllegalArgumentException("cannot generate negative Fibonacci number")
    else {
      n match {
        case n if n < 2 => 1
        case _ =>
          val f1 = Future {fib(n - 1)}
          val f2 = Future {fib(n - 2)}
          Await.result(f1, 2000.second) + Await.result(f2, 2000.second)
      }
    }
  }
}
