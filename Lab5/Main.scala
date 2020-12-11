import Tasks12._
import scala.collection.mutable

object Main
{
  def main(args:Array[String]):Unit =
  {
    testDuplicate()
    println()
    testDuplicateDistinct()
    println()
    
    val point = new Point(3,4)
    point.debugName()
    val vars = point.debugVars()
    println(vars)
  }

  def testDuplicate(): Unit =
  {
    var toDuplicate = mutable.Queue[Int](-2, -1, 0, 1, 2)
    var times = List(0, 3, 1, 4, 1)
    println(duplicate(toDuplicate, times) == mutable.Queue(-1, -1, -1, 0, 1, 1, 1, 1, 2))

    toDuplicate = mutable.Queue[Int](-2, -1, 0, 1, 2)
    times = List(1, 2, 3)
    println(duplicate(toDuplicate, times) == mutable.Queue(-2, -1, -1, 0, 0, 0))

    toDuplicate = mutable.Queue()
    times = List(1, 2, 3)
    println(duplicate(toDuplicate, times) == mutable.Queue())

    toDuplicate = mutable.Queue(1, 2, 3)
    times = List()
    println(duplicate(toDuplicate, times) == mutable.Queue())
  }

  def testDuplicateDistinct(): Unit =
  {
    var toDuplicate = mutable.LinkedHashSet[Int]()
    toDuplicate.addAll(List(-2, 1, 0, -1, 2, 2, 2))
    var times = mutable.Queue(0, 3, 1, 4, 1)
    println(duplicateDistinct(toDuplicate, times) == mutable.Queue(1, 1, 1, 0, -1, -1, -1, -1, 2))

    toDuplicate = mutable.LinkedHashSet[Int]()
    toDuplicate.addAll(List(-2, 1, 0, -1, 2, 2, 2))
    times = mutable.Queue(1, 2, 3)
    println(duplicateDistinct(toDuplicate, times) == mutable.Queue(-2, 1, 1, 0, 0, 0))

    toDuplicate = mutable.LinkedHashSet[Int]()
    times = mutable.Queue(1, 2, 3)
    println(duplicateDistinct(toDuplicate, times) == mutable.Queue())

    toDuplicate = mutable.LinkedHashSet[Int]()
    toDuplicate.addAll(List(1, 1, 2, 2, 3, 3))
    times = mutable.Queue()
    println(duplicateDistinct(toDuplicate, times) == mutable.Queue())
  }
}
