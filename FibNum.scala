import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object FibNum {
  def fibTail(n:Int):Int = {
    @tailrec
    def fibIter(f1: Int, f2: Int, m: Int): Int = {
      if (m == n) f1 + f2
      else fibIter(f2, f1 + f2, m + 1)
    }

    n match {
      case 0 => 0
      case 1 => 1
      case _ => if (n < 0) -1 else fibIter(0, 1, 2)
    }
  }

  def fib(n: Int): Int =
    n match {
      case 0 => 0
      case 1 => 1
      case _ => if (n < 0) -1 else fibTail(n - 2) + fibTail(n - 1)
    }

  def fibParallel(n: Int): Int =
    n match {
      case 0 => 0
      case 1 => 1
      case _ =>
        if (n < 0) -1
        else {
          val fibLeftF = Future{fibTail(n - 2)}
          val fibRightF = Future{fibTail(n - 1)}

          Await.result(fibLeftF, Duration.Inf) + Await.result(fibRightF, Duration.Inf)
        }
    }
}