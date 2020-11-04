package Lista4
import org.scalatest.FunSuite

class Lista4Test extends FunSuite{
  test("Lista 4 zadanie 1"){
    val tree1 = Lista4.createTree(2, 1, 10)
    val tree2 = Lista4.createTree(0, 1, 10)
    val tree3 = Lista4.createTree(3, 1, 10)
    val tree4 = Lista4.createTree(4, 1, 10)

    assert(Lista4.isTreeFull(tree1))
    assert(Lista4.isTreeFull(tree2))
    assert(Lista4.isTreeFull(tree3))
    assert(Lista4.isTreeFull(tree4))

    assert(Lista4.isTreeDepthRight(tree1, 2))
    assert(Lista4.isTreeDepthRight(tree2, 0))
    assert(Lista4.isTreeDepthRight(tree3, 3))
    assert(Lista4.isTreeDepthRight(tree4, 4))
  }

}
