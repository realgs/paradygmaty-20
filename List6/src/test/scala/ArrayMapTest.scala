import org.scalatest.FunSuite

import scala.util.Random

class ArrayMapTest extends FunSuite {
  test("squaresMap") {
    val in = Array(1, 2, 3, 4, 5, 6)
    val out: Array[Int] = Array.fill(6)(0)

    ArrayMap.map(in, out)(x => x * x)(parallel = false)

    assert(Array(1, 4, 9, 16, 25, 36) sameElements out)
  }

  def timeit[R](block: => R): Unit = {
    val start = System.currentTimeMillis()
    block
    val end = System.currentTimeMillis()
    printf("[timeit]: %s.", end - start)
  }

  test("time") {
    Random.setSeed(0)

    val length = 1000000
    val f = (x: Int) => x * x

    val arr = Array.fill(length)(Random.nextInt(50000))
    var arr_ = arr.clone()

    var out = Array.fill(length)(0)
    var out_ = out.clone()

    print("Seq: ")
    timeit(ArrayMap.map(arr, out)(f)(parallel = false))

    print("\nPar (4): ")
    timeit(ArrayMap.map(arr, out)(f)(parallel = true))
  }

  test("benchmark") {
    ArrayMap.benchmark()
  }
}
