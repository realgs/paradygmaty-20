import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object PrimesGenerator {
  def sequential(limit: Int): List[Int] = {
    var result = List[Int]()
    for (num <- Range(1, limit)) {
      if (isPrime(num)) {
        result = num :: result
      }
    }
    result.reverse
  }

  def concurrent(limit: Int): List[Int] = {
    val numOfThreads = Runtime.getRuntime.availableProcessors()
    val step = Math.ceil(limit.toDouble / numOfThreads).toInt
    var loops: List[Future[List[Int]]] = List()

    for (i <- Range(0, numOfThreads)) {
      val left = 1 + i * step
      val right = Math.min(limit, 1 + (i + 1) * step)
      loops = Future(getPrimesInRange(left, right)) :: loops
    }

    val partialResults: List[List[Int]] = Await.result(Future.sequence(loops), Duration.Inf)
    partialResults.foldRight(List[Int]())((a: List[Int], b: List[Int]) => b ::: a)
  }

  private def getPrimesInRange(left: Int, right: Int): List[Int] = {
    var result = List[Int]()
    for (num <- Range(left, right)) {
      if (isPrime(num)) {
        result = num :: result
      }
    }
    result.reverse
  }

  private def isPrime(number: Int): Boolean = {
    if (number == 1) false
    else if (number == 2) true
    else {
      var prime = true
      var i = 2
      while (prime && i * i <= number) {
        prime = number % i != 0
        i += 1
      }
      prime
    }
  }
}
