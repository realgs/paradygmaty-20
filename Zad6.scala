
import java.io.{FileNotFoundException, IOException}
import java.util.concurrent.{ForkJoinPool, ForkJoinTask, ForkJoinWorkerThread, RecursiveTask}

import scala.io.Source
import scala.collection.mutable.ListBuffer
import math.pow
import scala.collection.parallel.immutable.ParSeq
import scala.collection.parallel.CollectionConverters._
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.math.{max, min}
import scala.util.control.Breaks._


//poszukiwanie w plikie podobnych słów
class Text(val filename: String) {
  var text = read()
  var textPar: ParSeq[String] = text.par

  def read() = {
    val path = "C:\\Users\\leoni\\Desktop\\5 sem\\ios\\ZAD_6\\src\\main\\scala\\" + filename + ".txt"
    var list = new ListBuffer[String]()
    try {
      for (line <- Source.fromFile(path).getLines) {
        for (word <- line.split("[\"\'.,?! ]")) {
          list += word
        }
      }
    }
    catch {
      case e: FileNotFoundException => println("Couldn't find that file.")
      case e: IOException => println("Got an IOException!")
    }
    list.toList
  }

  def find_all(element: String, lev_distance: Int): Int = {
    var counter = 0
    text.foreach(e => {
      val result = distance(element, e)
      if (result < lev_distance) counter += 1
    })
    println("\n" + counter)
    counter
  }

  def parallel_find_all(element: String, lev_distance: Int): Int = {
    var counter = 0
    text.par.foreach(e => {
      val result = distance(element, e)
      if (result < lev_distance) counter += 1
    })
    println("\n" + counter)
    counter
  }


  def distance(x: String, y: String): Int = {
    if (x.isEmpty) return y.length
    if (y.isEmpty) return x.length
    val substitution = distance(x.substring(1), y.substring(1)) + costOfSubstitution(x.charAt(0), y.charAt(0))
    val insertion = distance(x, y.substring(1)) + 1
    val deletion = distance(x.substring(1), y) + 1

    min(min(substitution, insertion), deletion)

  }

  def costOfSubstitution(a: Char, b: Char): Int = if (a == b) 0
  else 1

}


class Pi {
  def plaff_pi(iterations: Int, start: Int = 0): BigDecimal = {
    var sum: BigDecimal = 0
    var counter = start
    while (counter < iterations) {
      val mult = 1.0 / pow(16.0, counter)
      val first: BigDecimal = 4.0 / (8 * counter + 1)
      val second: BigDecimal = 2.0 / (8 * counter + 4)
      val thert: BigDecimal = 1.0 / (8 * counter + 5)
      val fourth: BigDecimal = 1.0 / (8 * counter + 6)
      sum += mult * (first - second - thert - fourth)
      counter += 1
    }
    //println(sum)
    sum
  }


  def parallel_plaff_pi(iterations: Int): BigDecimal = {
    val first = Future {
      val result = plaff_pi((iterations / 3));
      result
    }
    val second = Future {
      val result = plaff_pi(iterations / 3 * 2, iterations / 3);
      result
    }
    val thert = Future {
      val result = plaff_pi(iterations, iterations / 3 * 2);
      result
    }

    val pi = Await.result(first, Duration.Inf) + Await.result(second, Duration.Inf) +
      Await.result(thert, Duration.Inf)
    //println(pi)
    pi
  }
}

class Prime {
  def prime_nums(n: Int, from: Int = 1) = {
    var list = ListBuffer[Int]()
    for (i <- from to n) {
      if (i == 2)
        list += 2
      else {
        var exists = false
        breakable {
          for (j <- 2 to i-1) {
            if (i % j == 0) {
              exists = true
              break()
            }
          }
        }
        if (!exists){
          list += i
        }
      }
    }
    list.toList
  }
  def parallel_prime_nums(n: Int) = {
    val first = Future {
      val result = prime_nums((n / 3));
      result
    }
    val second = Future {
      val result = prime_nums(n / 3 * 2, n / 3);
      result
    }
    val thert = Future {
      val result = prime_nums(n/4*3, n / 3 * 2);
      result
    }

    val forth = Future {
      val result = prime_nums(n, n/4*3);
      result
    }

    Await.result(first, Duration.Inf) :: Await.result(second, Duration.Inf) ::
      Await.result(thert, Duration.Inf) :: Await.result(forth, Duration.Inf)
  }
}

object Main extends App {
  //Do odczytu trzeba w metodzie read() zmienić path,
  //ponieważ jest ustawiony dla mojego komputera
  val text = new Text("Nostromo")

  var t1 = System.nanoTime
  text.find_all("NOSTROMO",2)
  var duration = (System.nanoTime - t1) / 1e9d
  println("\n"+duration)

  //kod z wykorzystaniem wbudowanych bibliotek do równoległego
  //przetwarzania średnie 2,6 razy szybszy, ale przykładowe jeżeli nie sprawdzaliśmy
  //odległości levenstaina , to czas w tym prypadku jest bardzo podobny,
  //czyli wykorzystanie zrównoleglenia jest potrzebne przy dużych strumieniach danych
  //i przy długo trwałych obliczeniach(wykorzystanych metod)
  t1 = System.nanoTime
  text.parallel_find_all("NOSTROMO",2)
  duration = (System.nanoTime - t1) / 1e9d
  println("\n"+duration)

  val pi = new Pi
  t1 = System.nanoTime
  for(i <- 0 to 10) {
    pi.plaff_pi(1000000)
  }
  duration = (System.nanoTime - t1) / 1e9d / 10
  println("\n" + duration)

  //kod z wykorzystaniem bibliotek Await i Future(nie wbudowane, modyfikacja sbt)
  //do równoległego przetwarzania średnie szybszy 2,5 razy przy 1.000.000 iteracji
  //przy mniejszej ilości iteracji szybszy 2,2 razy
  t1 = System.nanoTime
  for(i <- 0 to 10){
    pi.parallel_plaff_pi(1000000)
  }
  duration = (System.nanoTime - t1) / 1e9d / 10
  println("\n" + duration)

  val prime = new Prime

  t1 = System.nanoTime
  for(i <- 0 to 10) {
    prime.prime_nums(100000)
  }
  duration = (System.nanoTime - t1) / 1e9d /10
  println("\n" + duration)

  //kod z wykorzystaniem bibliotek Await i Future(nie wbudowane, modyfikacja sbt)
  //do równoległego przetwarzania średnie szybszy 2,2 przy poszukiwaniu 100.000,
  // przy  1000 różnica jest już w 1,7
  t1 = System.nanoTime
  for(i <- 0 to 10) {
    prime.parallel_prime_nums(100000)
  }
  duration = (System.nanoTime - t1) / 1e9d /10
  println("\n" + duration)

}