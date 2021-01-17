package Tests

import Tasks.Primes

object PrimeNumbersTest {

  def primesTest(): Unit = {
    timeTest(10, 100)
    timeTest(10, 1000)
    timeTest(10, 50000)
  }

  def timeTest(times: Int, number: Int): Unit = {
    var time = (0.toLong, 0.toLong)
    var currentTime = (0.toLong, 0.toLong)
    for (_ <- 1 to times) {
      currentTime = Utils.measureTime(Primes.generatePrimes(number), Primes.generatePrimesParallel(number))
      time = (time._1 + currentTime._1, time._2 + currentTime._2)
    }

    println("**********************************************************")
    println(times + " different runs of primes generator, from number: " + number)
    println("Average time for sequential primes generator: " + (currentTime._1 / times))
    println("Average time for parallel primes generator:   " + (currentTime._2 / times))
    println("**********************************************************\n")
  }
}
