import org.scalatest.FunSuite
import ClosestPairOfPoints.ClosestPairOfPoints._
import ClosestPairOfPoints.Point

class ClosestPairOfPointsTest extends FunSuite {

  test("ClosestPairOfPoints - closestPairOfPoints correctness test 1") {
    val p1 = new Point(2, 3)
    val p2 = new Point(12, 30)
    val p3 = new Point(40, 50)
    val p4 = new Point(5, 1)
    val p5 = new Point(12, 10)
    val p6 = new Point(3, 4)
    val arr = Array(p1, p2, p3, p4, p5, p6)

    assert(1.4142 == closestPairOfPoints(arr))
  }

  test("ClosestPairOfPoints - closestPairOfPoints correctness test 2") {
    val p1 = new Point(4, 1)
    val p2 = new Point(15, 20)
    val p3 = new Point(30, 40)
    val p4 = new Point(8, 4)
    val p5 = new Point(13, 11)
    val p6 = new Point(5, 6)
    val arr = Array(p1, p2, p3, p4, p5, p6)

    assert(3.6056 == closestPairOfPoints(arr))
  }

  test("ClosestPairOfPoints - closestPairOfPointsFuture correctness test 1") {
    val p1 = new Point(2, 3)
    val p2 = new Point(12, 30)
    val p3 = new Point(40, 50)
    val p4 = new Point(5, 1)
    val p5 = new Point(12, 10)
    val p6 = new Point(3, 4)
    val arr = Array(p1, p2, p3, p4, p5, p6)

    assert(1.4142 == closestPairOfPointsFuture(arr))
  }

  test("ClosestPairOfPoints - closestPairOfPointsFuture correctness test 2") {
    val p1 = new Point(4, 1)
    val p2 = new Point(15, 20)
    val p3 = new Point(30, 40)
    val p4 = new Point(8, 4)
    val p5 = new Point(13, 11)
    val p6 = new Point(5, 6)
    val arr = Array(p1, p2, p3, p4, p5, p6)

    assert(3.6056 == closestPairOfPointsFuture(arr))
  }

  test("ClosestPairOfPoints - closestPairOfPointsParallel correctness test 1") {
    val p1 = new Point(2, 3)
    val p2 = new Point(12, 30)
    val p3 = new Point(40, 50)
    val p4 = new Point(5, 1)
    val p5 = new Point(12, 10)
    val p6 = new Point(3, 4)
    val arr = Array(p1, p2, p3, p4, p5, p6)

    assert(1.4142 == closestPairOfPointsParallel(arr))
  }

  test("ClosestPairOfPoints - closestPairOfPointsParallel correctness test 2") {
    val p1 = new Point(4, 1)
    val p2 = new Point(15, 20)
    val p3 = new Point(30, 40)
    val p4 = new Point(8, 4)
    val p5 = new Point(13, 11)
    val p6 = new Point(5, 6)
    val arr = Array(p1, p2, p3, p4, p5, p6)

    assert(3.6056 == closestPairOfPointsParallel(arr))
  }

}
