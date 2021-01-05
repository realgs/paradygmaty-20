import org.scalatest.flatspec.AnyFlatSpec
import ArraySum._

class ArraySumTest extends AnyFlatSpec {
  "Array sum" should "correctly calculate sum of all array elements" in {
    assert(calculateSum(Array(1, 2, -100, 1000)) == 903)
  }

  "Array sum" should "return 0 for empty array" in {
    assert(calculateSum(Array[Int]()) == 0)
  }

  "Array sum parallel" should "correctly calculate sum of all array elements" in {
    assert(parCalculateSum(Array(1, 2, -100, 1000)) == 903)
  }

  "Array sum parallel" should "return 0 for empty array" in {
    assert(parCalculateSum(Array[Int]()) == 0)
  }
}
