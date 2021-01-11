package Algorithms

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Fibonacci {
  def findFibonacci(n: Int): BigInt = {
    n match {
      case 0 => 0
      case 1 => 1
      case _ => findFibonacci(n - 1) + findFibonacci(n - 2)
    }
  }

  def findFibonacciIter(n: Int): BigInt = {
    @scala.annotation.tailrec
    def fibInner(n: Int, f1: Int, f2: Int): BigInt = {
      n match {
        case 0 => f1
        case 1 => f2
        case _ => fibInner(n - 1, f2, f1 + f2)
      }
    }

    if (n >= 0) fibInner(n, 0, 1)
    else throw new Exception("n must be positive")
  }

  def findFibonacciParallel(n: Int, depth: Int): BigInt = {
    if (depth == 0) findFibonacci(n)
    else {
      n match {
        case 0 => 0
        case 1 => 1
        case _ =>
          val f1 = Future(findFibonacciParallel(n - 1, depth - 1))
          val f2 = Future(findFibonacciParallel(n - 2, depth - 1))
          Await.result(f1, 1000.seconds) + Await.result(f2, 1000.seconds)
      }
    }
  }
}
