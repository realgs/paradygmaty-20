import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object FibonacciNumb {

  def findFibNumber(n: Int): Int = {
    if (n < 0) throw new Exception (s"Negative n: $n")
    else {
      n match {
        case 0 => 0
        case 1 => 1
        case _ => findFibNumber(n - 1) + findFibNumber(n - 2)
      }
    }
  }

  def findFibNumbTailRec(n: Int): Int =
    if(n < 0) throw new Exception(s"Negative n: $n")
    else {
      @scala.annotation.tailrec
      def fibAssistant(n: Int, a: Int, b: Int): Int =
        n match {
          case 0 => a
          case _ => fibAssistant(n - 1, b, a + b)
        }
      fibAssistant(n, 0 , 1)
    }

  def findFibNumbParallel(n: Int, depth: Int): Int = {
    if (n < 0) throw new Exception (s"Negative n: $n")
    else if (depth == 0)
      findFibNumber(n)
    else {
      n match {
        case 0 => 0
        case 1 => 1
        case _ =>
          val futureA = Future(findFibNumbParallel(n - 1, depth - 1))
          val futureB = Future(findFibNumbParallel(n - 2, depth - 1))
          Await.result(futureA, 10.minutes) + Await.result(futureB, 10.minutes)
      }
    }
  }

}
