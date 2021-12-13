import org.scalatest.FunSuite

class MonteCarloTest extends FunSuite {
  val epsilon = 0.001

  test("piSeqCertainty") {
    assert((MonteCarlo.piSeq(10000000) - Math.PI).abs < epsilon)
  }

  test("piParCertainty") {
    assert((MonteCarlo.piPar(10000000) - Math.PI).abs < epsilon)
  }

  test("benchmark") {
    MonteCarlo.benchmark()
  }
}
