import org.scalatest.FunSuite

import scala.util.Random

class QuicksortTest extends FunSuite {
  test("randomArray") {
    val arr = Array.fill(100000)(Random.nextInt(50000))
    var arr_ = arr.clone()

    Quicksort.sort(arr, parallel = true)
    arr_ = arr_.sorted

    assert(arr sameElements arr_)
  }

  test("benchmark") {
    Quicksort.benchmark()
  }

  def timeit[R](block: => R): Unit = {
    val start = System.currentTimeMillis()
    block
    val end = System.currentTimeMillis()
    printf("[timeit]: %s.", end - start)
  }
}
