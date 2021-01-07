package ClosestPairOfPoints
import ParallelMachine.ParallelMachine._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, DurationInt}
import scala.concurrent.{Await, Future}
import scala.math.sqrt
import scala.math.BigDecimal

//I would normally put this class in separate file, but it has only 3 lines and it is only used in this algorithm
class Point(var x:Double = 0.0, var y:Double = 0.0) {
  override def toString = s"[$x, $y]"
}

object ClosestPairOfPoints {
  //returns distance between two points
  private def distance(p1: Point, p2: Point): Double =
    sqrt(((p1.x - p2.x) * (p1.x - p2.x)) + ((p1.y - p2.y) * (p1.y - p2.y)))

  private def shortestDistForFirstInterval(tabLeft: Array[Point], tabRight: Array[Point]): Double = {
    var minDistance = Double.MaxValue
    for(i <- tabLeft.indices) {
      for(j <- i+1 until tabRight.length) {
        if(distance(tabLeft(i), tabRight(j)) < minDistance) {
          minDistance = distance(tabLeft(i), tabRight(j))
        }
      }
    }
    // rounding double with 0.0001 accuracy
    BigDecimal(minDistance).setScale(4, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  private def shortestDistForSecondInterval(tabLeft: Array[Point], tabRight: Array[Point], midIndex: Int): Double = {
    var minDistance = Double.MaxValue
    for(i <- tabLeft.indices) {
      for(j <- tabRight.indices) {
        if(distance(tabLeft(i), tabRight(j)) < minDistance && i != j + midIndex ) { // i != j + midIndex - because we dont want to compare point with other
          minDistance = distance(tabLeft(i), tabRight(j))
        }
      }
    }
    // rounding double with 0.0001 accuracy
    BigDecimal(minDistance).setScale(4, BigDecimal.RoundingMode.HALF_UP).toDouble
  }


  def closestPairOfPoints(tab: Array[Point]): Double = {
    shortestDistForFirstInterval(tab, tab)
  }


  def closestPairOfPointsFuture(tab: Array[Point]): Double = {
    if(tab.length <= 5)
      shortestDistForFirstInterval(tab, tab)
    else {
      val midIndex = tab.length / 2
      val leftTab = new Array[Point](midIndex)
      val rightTab = new Array[Point](tab.length - midIndex)
      for(i <- 0 until midIndex) {
        leftTab(i) = tab(i)
      }
      var j = 0
      for(i <- midIndex until tab.length) {
        rightTab(j) = tab(i)
        j += 1
      }
      val fut1 = Future(shortestDistForFirstInterval(tab, leftTab))
      val fut2 = Future(shortestDistForSecondInterval(tab, rightTab, midIndex))
      val result1 = Await.result(fut1, Duration.Inf)  //safe in this case, because i know that function will return at some point
      val result2 = Await.result(fut2, Duration.Inf)  //blocking for certain amount of time could cause returning incorrect pair of points

      if(result1 < result2)
        result1
      else
        result2
    }
  }

  def closestPairOfPointsParallel(tab: Array[Point]): Double = {
    if(tab.length <= 5)
      shortestDistForFirstInterval(tab, tab)
    else {
      val midIndex = tab.length / 2
      val (leftTab, rightTab) = tab.clone().splitAt(midIndex)
      val (result1, result2) = parallel(shortestDistForFirstInterval(tab, leftTab), shortestDistForSecondInterval(tab, rightTab, midIndex))
      if(result1 < result2)
        result1
      else
        result2
    }
  }
}
