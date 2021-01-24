import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

object L6_pattern {

  def my_contains(x: String, y: String): Boolean = {
    def containsIter(xs: List[Char], ys: List[Char]): Boolean = {
      (xs, ys) match {
        case (Nil, Nil) => true
        case (Nil, _) => false
        case (_, Nil) => true
        case (xs, ys) => if (ys.head == xs.head) containsIter(xs.tail, ys.tail)
        else containsIter(xs.tail, y.toList)
      }
    }
    containsIter(x.toList, y.toList)
  }

  def findSingle[A](y: A, xs: List[A]): List[A] = {
    if(xs.nonEmpty) {
      if (my_contains(xs.head.toString, y.toString)) xs.head :: findSingle(y, xs.tail)
      else findSingle(y, xs.tail)
    }else Nil
  }

  //Not Parallel
  def find[A](ys: List[A], xs: List[A]): List[A] =
    if(ys.isEmpty) Nil
    else findSingle(ys.head, xs):::find(ys.tail, xs)

  //Parallel
  def findPar[A](ys: List[A], xs: List[A]): List[A] = {
    if (ys.isEmpty) Nil
    else {
      val (first, second) = xs.splitAt(xs.length / 2)
      val fr = Future(findSingle(ys.head, first))
      val sr = Future(findSingle(ys.head, second))
      Await.result(fr, Duration.Inf) ::: Await.result(sr, Duration.Inf) ::: findPar(ys.tail, xs)
    }
  }
}






