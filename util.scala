import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

package object util {

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println("Working time: " + (t1 - t0)/1000000 + "ms")
    result
  }

  def listDivide[T](xs: List[T]): (List[T], List[T]) = {
    def getFirstN[T](xs: List[T], n: Int): List[T] =
      n match{
        case 0 => Nil
        case _ => xs.head :: getFirstN(xs.tail, n - 1)
      }
    def getFromN[T](xs: List[T], n: Int): List[T] =
      n match{
        case 0 => xs
        case _ => getFromN(xs.tail, n - 1)
      }
    val div = xs.length / 2
    (getFirstN(xs, div), getFromN(xs, div))
  }

  def merge[T](f: (T, T) => Boolean, xs1: List[T], xs2: List[T]): List[T] = {
    @tailrec
    def helper[T](f: (T, T) => Boolean, xs1: List[T], xs2: List[T], acum: List[T]): List[T] = {
      (xs1, xs2) match {
        case (Nil, _) => acum ++ xs2
        case (_, Nil) => acum ++ xs1
        case (h1 :: t1, _) => if (f(h1, xs2.head)) helper(f, xs1.tail, xs2, acum :+ xs1.head)
        else helper(f, xs1, xs2.tail, acum :+ xs2.head)
      }
    }
    helper(f, xs1, xs2, List())
  }

  def parallel[A, B](taskA: =>A, taskB: =>B): (A,B) = {
    val future: Future[B] = Future { taskB }  // starts computing taskB on different thread
    val a: A = taskA
    val b: B = Await.result(future, Duration.Inf) // waiting for future to give result of taskB
    (a,b)
  }

  def arraysEquality(arr1: Array[Int], arr2: Array[Int]): Boolean={
    if(arr1.length != arr2.length) false
    else{
      for(i <- 0 to arr1.length - 1){
        if(arr1(i) != arr2(i))
          false
      }
      true
    }
  }

  def randomListGen(length: Int, range: (Int, Int) = (0,100)): List[Int] ={
    List.fill(length)(Random.between(range._1, range._2))
  }
}