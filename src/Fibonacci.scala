import ParallelMechanism.parallel

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object Fibonacci {
  def fib(n : Int): BigInt =
    n match {
      case 0 => 0
      case 1 => 1
      case _ => fib(n - 1) + fib(n - 2)
    }

  def fibParallel(n: Int): BigInt = {
    n match {
      case 0 => 0
      case 1 => 1
      case _ =>
        if(n <= 10) fib(n - 1) + fib(n - 2)
        else {
          val result = parallel(fib(n - 1), fib(n - 2))
          result._1 + result._2
        }
    }
  }

  def fibFuture(n: Int): BigInt = {
    n match {
      case 0 => 0
      case 1 => 1
      case _ =>
        if(n <= 10) fib(n - 1) + fib(n - 2)
        else {
          val fib1 = Future(fib(n - 1))
          val fib2 = Future(fib(n - 2))
          Await.result(fib1, 2000.second) + Await.result(fib2, 2000.second)
        }
    }
  }

  def fibTail(n: Int): BigInt = {
    @tailrec
    def fibHelper(n: Int, fib1: Int, fib2: Int): BigInt = {
      n match {
        case 0 => fib1
        case 1 => fib2
        case _ => fibHelper(n - 1, fib2, fib1 + fib2)
      }
    }
    if(n < 0) throw new Exception("n cant be negative")
    else fibHelper(n, 0, 1)
  }
}

