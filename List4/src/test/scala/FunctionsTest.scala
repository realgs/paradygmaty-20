import org.scalatest.FunSuite

class FunctionsTest extends FunSuite {
  test("tree.test") {
    val t = Functions.generateTree(2, 0, 25)
    print(t)
  }

  test("tree.diff") {
    val diff = (lhs: BTree[Int], rhs: BTree[Int]) => {
    }

    val t1 = Functions.generateTree(4, 0, 25)
    val t2 = Functions.generateTree(4, 0, 25)

    Functions.elementwiseDifference(t1, t2)(???)
  }
}
