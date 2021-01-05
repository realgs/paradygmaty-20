import Parallel._
import org.scalameter._


import scala.util.Random

object ArraySum {
  private def sum(array: Array[Int], startIndex: Int, endIndex: Int): Int = {
    var sum: Int = 0
    for (i <- Range(startIndex, endIndex)) sum = sum + array(i)
    sum
  }

  def calculateSum(array: Array[Int]): Int = {
    if (array.isEmpty) 0
    else sum(array, 0, array.length)
  }

  def parCalculateSum(array: Array[Int]): Int = {
    if (array.isEmpty) 0
    else {
      val result = parallel(sum(array, 0, array.length / 2), sum(array, array.length / 2, array.length))
      result._1 + result._2
    }
  }

  def main(args: Array[String]): Unit = {
    val array = Array.fill(100000000)(new Random().nextInt(10))

    Thread.sleep(500)

    val time1 = measure {
      parCalculateSum(array)
    }

    val time2 = measure {
      calculateSum(array)
    }

    println(time1)
    println(time2)
  }
}
