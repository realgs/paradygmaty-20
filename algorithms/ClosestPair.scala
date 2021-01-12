// Adrian Chłopowiec
package algorithms

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

class Point(var x: Double, var y: Double)
{
  def distance(other: Point): Double =
    {
      math.sqrt(math.pow(this.x - other.x, 2) - math.pow(this.y - other.y, 2))
    }
}

object ClosestPair
{
  def closestDistance(arr: Array[Point]): Double =
    {
      //val points = QuicksortParallelPoints.quicksort(arr)
      val points = arr.sortWith((p1, p2) => p1.x < p2.x)
      val d = smallestDistSub(arr, 0, points.length - 1)

      val strip = points filter ((p => math.abs(p.x) < d))
      strip.sortWith((p1, p2) => p1.y < p2.y)
      math.min(d, stripClosest(strip, strip.length, d))
    }

  private def smallestDistSub(points: Array[Point], start: Int, end: Int): Double =
  {
    if(end - start <= 3)
      return bruteForceDist(points, start, end)

    val mid = (start + end) / 2
    val dl = smallestDistSub(points, 0, mid)
    val dr = smallestDistSub(points, mid + 1, end)
    math.min(dl, dr)
  }

  private def bruteForceDist(points: Array[Point], start: Int, end: Int): Double =
    {
      var min = Double.MaxValue
      for(i <- start to end)
        {
          for(j <- i + 1 to end)
            {
              val dist = points(i).distance(points(j))
              if(dist < min)
                min = dist
            }
        }
      min
    }

  private def stripClosest(strip: Array[Point], size: Int, distance: Double): Double =
    {
      var minVal = distance

      for(i <- 0 until size)
        {
          var j = i + 1
          while(j < size && (strip(j).y - strip(i).y) < minVal)
            {
              minVal = strip(i).distance(strip(j))
              j += 1
            }
        }
      minVal
    }
}

object ClosestPairParallel
{
  def closestDistance(arr: Array[Point]): Double =
  {
    val points = arr.sortWith((p1, p2) => p1.x < p2.x)
    val d = smallestDistSubPar(arr, 0, points.length - 1, parallelDepth())

    val strip = points filter ((p => math.abs(p.x) < d))
    strip.sortWith((p1, p2) => p1.y < p2.y)
    math.min(d, stripClosest(strip, strip.length, d))
  }

  private def parallelDepth(): Int =
  {
    val processors = Runtime.getRuntime.availableProcessors()
    val binary = processors.toBinaryString
    binary.length - binary.indexOf(1)
  }

  private def smallestDistSubPar(points: Array[Point], start: Int, end: Int, depth: Int): Double =
  {
    if(end - start <= 3)
      return bruteForceDist(points, start, end)

    val mid = (start + end) / 2
    var dl = 0.0
    var dr = 0.0
    if(depth > 0)
      {
        val leftFuture = Future { smallestDistSubPar(points, 0, mid, depth - 1)}
        val rightFuture = Future { smallestDistSubPar(points, mid + 1, end, depth - 1)}
        dl = Await.result(leftFuture, 1000.seconds)
        dr = Await.result(rightFuture, 1000.seconds)
      }
    else
      {
        dl = smallestDistSub(points, 0, mid)
        dr = smallestDistSub(points, mid + 1, end)
      }

    math.min(dl, dr)
  }

  private def smallestDistSub(points: Array[Point], start: Int, end: Int): Double =
  {
    if(end - start <= 3)
      return bruteForceDist(points, start, end)

    val mid = (start + end) / 2
    val dl = smallestDistSub(points, 0, mid)
    val dr = smallestDistSub(points, mid + 1, end)
    math.min(dl, dr)
  }

  private def bruteForceDist(points: Array[Point], start: Int, end: Int): Double =
  {
    var min = Double.MaxValue
    for(i <- start to end)
    {
      for(j <- i + 1 to end)
      {
        val dist = points(i).distance(points(j))
        if(dist < min)
          min = dist
      }
    }
    min
  }

  private def stripClosest(strip: Array[Point], size: Int, distance: Double): Double =
  {
    var minVal = distance

    for(i <- 0 until size)
    {
      var j = i + 1
      while(j < size && (strip(j).y - strip(i).y) < minVal)
      {
        minVal = strip(i).distance(strip(j))
        j += 1
      }
    }
    minVal
  }
}


