package List6

import List6.Parallel.parallel
import scala.annotation.tailrec

object PrimeNumbers {

  //Searching all prime numbers, which are lower or equal n
  def searchPrime(n: Int): List[Int] = {
    searchPrime(1, n, List()).reverse
  }

  def parSearchPrime(n: Int): List[Int] = {
    val mid = n / 2
    val (left, right) = parallel(searchPrime(0, mid, List()), searchPrime(mid+1, n, List()))
    left.reverse ::: right.reverse
  }

  def ifPrime(number: Int): Boolean = {
    if (number < 2) false
    else {
      for(x <- 2 to number-1) {
        if(number % x == 0) return false
      }
      true
    }
  }

  @tailrec
  def searchPrime(from: Int, until: Int, list: List[Int]): List[Int] = {
    //var primeList = List.fill(n)(1 until(n))
    if(from > until) list
    else if(ifPrime(from)) searchPrime(from+1, until, from :: list)
    else searchPrime(from+1, until, list)
  }

}
