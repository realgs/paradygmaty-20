import org.scalameter._
import scala.util.Random

object Utilities {
  val random: Random.type = scala.util.Random

  /*def timeMeasure[A](block: => A): Quantity[Double] = {
    val time = measure(block)
    time
  }*/

  def timeMeasureMilliSeconds[A](block: => A): Unit = {
    val t0 = System.currentTimeMillis()
    block
    val t1 = System.currentTimeMillis()
    println("Total time: " + (t1 - t0) + "ms")
  }

  def timeMeasureNanoSeconds[A](block: => A): Unit = {
    val t0 = System.nanoTime()
    block
    val t1 = System.nanoTime()
    println("Total time: " + (t1 - t0) + "ns")
  }

}
