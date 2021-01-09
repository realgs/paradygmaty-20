package Tasks

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object Primes {

  def generatePrimes(n: Int): List[Int] = {
    if (n < 1) throw new IllegalArgumentException("cannot generate primes lower than 1")
    else generatePrimesFromTo(1, n)
  }

  def generatePrimesParallel(n: Int): List[Int] = {
    if (n < 1) throw new IllegalArgumentException("cannot generate primes lower than 1")
    else {
      val p1 = Future {
        generatePrimesFromTo(1, n / 2)
      }
      val p2 = Future {
        generatePrimesFromTo((n / 2) + 1, n)
      }
      Await.result(p1, 100.seconds) ::: Await.result(p2, 100.seconds)
    }
  }

  private def generatePrimesFromTo(start: Int, end: Int): List[Int] = {
    if (start > end) Nil
    else if (isPrime(start)) start :: generatePrimesFromTo(start + 1, end)
    else generatePrimesFromTo(start + 1, end)
  }

  private def isPrime(number: Int): Boolean = {
    if (number < 2) return false
    else {
      for (x <- 2 until number) {
        if (number % x == 0) return false
      }
      return true
    }
  }
}
