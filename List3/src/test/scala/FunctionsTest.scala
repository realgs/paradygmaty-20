import org.scalatest.FunSuite

class FunctionsTest extends FunSuite {
  assert(Functions.split(List(-3, -6, 8, -9, 13)) === (List(-3, -6, -9),List(-3, -9)))
}
