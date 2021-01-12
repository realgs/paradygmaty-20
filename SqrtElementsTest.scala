import Benchmark.calculateExecutionTime
import org.scalatest.FunSuite

import scala.util.Random

/**
 * Benchmark
 *
 * Test 100_000_000
 * SqrtElements.sequential() took 207ms
 * SqrtElements.parallel() took 105ms
 *
 * Test 10_000_000
 * SqrtElements.sequential() took 31ms
 * SqrtElements.parallel() took 17ms
 *
 * Test 1_000_000
 * SqrtElements.sequential() took 9ms
 * SqrtElements.parallel() took 8ms
 *
 * Test 100_000
 * SqrtElements.sequential() took 4ms
 * SqrtElements.parallel() took 4ms
 */
class SqrtElementsTest extends FunSuite {
  private val ARRAY_TEST_SIZE = 1_000_000
  private final val array = Array.fill(ARRAY_TEST_SIZE)(Random.nextDouble())

  test("SqrtElementsSequentialTest") {
    val arrayCopy = array.clone()
    SqrtElements.sequential(arrayCopy)
    assert(checkSqrtCorrectness(arrayCopy))
  }

  test("SqrtElementsParallelTest") {
    val arrayCopy = array.clone()
    SqrtElements.parallel(arrayCopy)
    assert(checkSqrtCorrectness(arrayCopy))
  }

  test("SqrtElementsBenchmarkTest") {
    val arrayCopy1 = array.clone()
    val arrayCopy2 = array.clone()

    calculateExecutionTime("SqrtElements.sequential()") {
      SqrtElements.sequential(arrayCopy1)
    }
    calculateExecutionTime("SqrtElements.parallel()") {
      SqrtElements.parallel(arrayCopy2)

    }
  }

  def checkSqrtCorrectness(array: Array[Double]): Boolean = array.indices.forall(i => array(i) == Math.sqrt(this.array(i)))
}
