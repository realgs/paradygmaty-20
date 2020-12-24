import org.scalatest.FunSuite

class PrimesGeneratorTest extends FunSuite {
  test("sequential") {
    assert(PrimesGenerator.sequential(1) == List())
    assert(PrimesGenerator.sequential(2) == List())
    assert(PrimesGenerator.sequential(3) == List(2))
    assert(PrimesGenerator.sequential(50) == List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47))
  }

  test("concurrent") {
    assert(PrimesGenerator.concurrent(1) == List())
    assert(PrimesGenerator.concurrent(2) == List())
    assert(PrimesGenerator.concurrent(3) == List(2))
    assert(PrimesGenerator.concurrent(50) == List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47))
  }

  test("efficiency") {
    val limit = 10_000_000
    println(s"Sequential: ${Utils.executionTime(() => PrimesGenerator.sequential(limit))} ms")
    println(s"Concurrent: ${Utils.executionTime(() => PrimesGenerator.concurrent(limit))} ms")

    /*
     * Sequential: 10301 ms
     * Concurrent: 2444 ms
     */
  }
}
