package runtime;
package immutable;
import java.util.concurrent.{ForkJoinPool, ForkJoinTask, ForkJoinWorkerThread, RecursiveTask}

import jdk.jfr.Threshold

import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.immutable.ParMap
import scala.collection.parallel.immutable.ParHashMap
import scala.util.Random
import scala.collection.parallel.CollectionConverters._
import akka.actor.{Actor, ActorSystem, Props, Scheduler}
import scala.language.postfixOps
import scala.util.DynamicVariable

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalameter
import akka.actor.{ActorContext, ActorSystem, Actor, Props}

object Parallele extends App{

  //Future and await test
  // val f = Future{ "hello" + 123 + "world" };
  val f = Future{ Thread.sleep(1000); "hello" + 123 + "world" }
  Await.result(f, Duration.Inf)
  println(f);

  def slowFunction(i: Int) = { Thread.sleep(1000); "hello" + i + "world" }
  println(slowFunction(123))
  println(slowFunction(456))

  def method(x: Int): Future[Int] = Future {
    x * x
  }

  def doParallel(x: Int, y: Int, z: Int): Future[List[Int]] = {
    val result1 = method(x)
    val result2 = method(y)
    val result3 = method(z)
    val result = for {
      r1 <- result1
      r2 <- result2
      r3 <- result3
    } yield List(r1, r2, r3)
    result
  }

  val result = Await.result(doParallel(2, 3, 4), Duration.Inf);

  println("result = " + result);
  println("_______________________________" + "\n")

  //Fibonacci parallel

  def fibonacciSumOfValue(value: Int): Int = {
    require(value >= 0, s"Negative number $value!");
    value match {
        case value if value < 2 => 1
        case _ => fibonacciSumOfValue(value - 1) + fibonacciSumOfValue(value - 2)
    }
  }

  val time1 = System.nanoTime();
  println("fibonacci sum = " + fibonacciSumOfValue(32));
  val time2 = System.nanoTime();
  println("Time result for fibonacciSumOfValue = " + (time2 - time1)) ;


  def fibonacciSumOfValueParallel(value: Int): Int = {
    require(value > 0, s"Negative number $value!");

    value  match {
        case value  if value  < 2 => 1
        case _ =>
          val f1 = Future {fibonacciSumOfValue(value  - 1)}
          val f2 = Future {fibonacciSumOfValue(value  - 2)}
          Await.result(f1, 2000.second) + Await.result(f2, Duration.Inf)
      }
  }
  val time3 = System.nanoTime();
  println("fibonacciParallel sum = " + fibonacciSumOfValueParallel(32))
  val time4 = System.nanoTime();
  println("Time result for fibonacciSumOfValueParallel = " + (time4 - time3)) ;
  println("_______________________________" + "\n")

//Parallel for quickSort
  def quicksort(data: List[Int]): List[Int] = {
    if (data.isEmpty) {
      data
    } else {
      val pivot = data.head
      val (leftP, rightP) = data.tail partition (_ < pivot)
      quicksort(leftP) ++ (pivot :: quicksort(rightP))
    }
  }

  val list:List[Int] = List(4,6,2,6,3,10);
  println(quicksort(list));

  val THRESHOLD = 2000;//for test
  def quicksortParallel(data: List[Int]): List[Int] = {
    if (data.size < THRESHOLD) quicksort(data)
    else {
      val pivot = data.head
      val (leftP, rightP) = data.tail partition (_ < pivot)
      val f_value = for {
        left <- Future(quicksortParallel(leftP))
        right <- Future(quicksortParallel(rightP))
      } yield left ++ (pivot :: right)

      Await.result(f_value, Duration.Inf)
    }
  }

  def time[A](block: => A)(name: String = "NoName"): A = {
    val start = System.nanoTime()
    val result = block
    val end = System.nanoTime()
    println(s"czas: ${(end - start) / 1000} dla $name")
    result
  }

  //test quickSort dla duzego ciagu liczb
  val testData = List.fill(5000)(Random.nextInt)
  println("Dlugosc listy = " + testData.length);
  val result1 = time {
    quicksort(testData)
  }("quicksort")
  val result2 = time {
    quicksortParallel(testData)
  }("quicksortParallel")
  if (result1 == result2) println("done") else println("fail")

  println();
  println("_______________________________" + "\n")


//BinaryTree generator
object BT {

  sealed trait BT[+A]

  case object Empty extends BT[Nothing]

  case class Node[+A](value: A, left: BT[A], right: BT[A]) extends BT[A]

  def generateTree(depth: Int, range:Int): BT[Int] = {

    def toPar(value:Int):BT[Int]=
              if(depth == 0){
                Node(0, Empty, Empty);
              }else {
                val tree1 = Future {
                  Node(Random.nextInt(range), generateTree(depth - 1, range), generateTree(depth - 1, range))
                };
                val tree2 = Future {
                  Node(Random.nextInt(range), generateTree(depth - 1, range), generateTree(depth - 1, range))
                };
                val one = Await.result(tree1, Duration.Inf)
                val two = Await.result(tree2, Duration.Inf)

                Node(Random.nextInt(range), one, two)
              }
    if(depth >= 0) Node(0, Empty, Empty);
    toPar(depth - 1);
  }

  def genTree(depth: Int, range:Int): BT[Int] = {
    if(depth >= 0){
      Node(Random.nextInt(range),genTree(depth - 1,range),genTree(depth - 1,range));
    }else return Node(0, Empty, Empty);
  }

}

    val time5 = System.nanoTime()
    BT.genTree(5, 7)
    val time6 = System.nanoTime()

    val time7 = System.nanoTime()
    BT.generateTree(5, 8)
    val time8 = System.nanoTime()

    println("Binary Tree: " + (time6 - time5))
    println("Parallel Binary Tree: " + (time8 - time7))
}
