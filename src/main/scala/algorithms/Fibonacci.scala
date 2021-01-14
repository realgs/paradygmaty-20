package algorithms

import java.util.concurrent.ForkJoinTask.invokeAll
import java.util.concurrent.{ForkJoinPool, RecursiveTask}

object Fibonacci {
  def sequentially(n: Int): Long =
    n match {
      case 0 => 0
      case 1 => 1
      case _ => sequentially(n - 1) + sequentially(n - 2)
    }

  def parallel(n: Int): Long = {
    new ForkJoinPool().invoke(new FibonacciParallel(n))
  }

  class FibonacciParallel(n: Int) extends RecursiveTask[Long] {
    private val FIBONACCI_NUMBER_THRESHOLD = 25

    override def compute(): Long = n match {
      case 0 => 0
      case 1 => 1
      case _ => {
        if (n <= FIBONACCI_NUMBER_THRESHOLD) {
          sequentially(n - 1) + sequentially(n - 2)
        }
        else {
          val firstNumber = new FibonacciParallel(n - 1)
          val secondNumber = new FibonacciParallel(n - 2)

          invokeAll(firstNumber, secondNumber)
          firstNumber.get() + secondNumber.get()
        }
      }
    }
  }

}
