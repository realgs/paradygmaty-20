import org.scalatest.FunSuite

import scala.util.Random

class ArrayReduceTest extends FunSuite {

  def timeit[R](block: => R): Unit = {
    val start = System.currentTimeMillis()
    block
    val end = System.currentTimeMillis()
    printf("[timeit]: %s.", end - start)
  }

  test("compareSumWithReduce") {
    val xs = Array.fill(10000)(Random.nextInt(500))
    assert(ArrayReduce.reduceParallel(xs)(_ + _) === xs.sum)
  }

  test("benchmark") {
    ArrayReduce.benchmark()
  }
}
