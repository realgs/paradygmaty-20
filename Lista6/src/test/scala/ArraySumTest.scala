import org.scalatest.FunSuite

class ArraySumTest extends FunSuite{

  test("sum_test"){
    val array = Array[Int](1,2,4,5,6,7,7,8,8,9,10)
    assert(ArraySum.sum(array) === 67)
  }

  test("parallelSum_test"){
    val array = Array[Int](1,2,4,5,6,7,7,8,8,9,10)
    assert(ArraySum.parallelSum(array) === 67)
  }

  test("parallelSum_test_ones"){
    val array = Array[Int](1,1,1,1,1,1,1,1,1,1)
    assert(ArraySum.parallelSum(array) === 10)
  }
}
