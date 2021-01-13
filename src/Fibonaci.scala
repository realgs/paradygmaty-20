import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object Fibonaci {
  def fibSimple(n: Int): BigInt =
    n match {
      case 0 => 0
      case 1 => 1
      case _ => fibSimple(n - 1) + fibSimple(n - 2)
    }

  def fibParallel(n: Int): BigInt = {
    n match {
      case 0 => 0
      case 1 => 1
      case _ =>
        val fib1 = Future(fibSimple(n - 1))
        val fib2 = Future(fibSimple(n - 2))
        Await.result(fib1, 1000.second) + Await.result(fib2, 1000.second)
    }
  }
}
