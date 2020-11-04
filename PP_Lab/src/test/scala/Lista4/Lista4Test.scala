package Lista4
import org.scalatest.FunSuite

class Lista4Test extends FunSuite{
  test("Lista 4 zadanie 1"){
    assert(Lista4.isTreeFull(Lista4.createTree(2, 1, 10)))
    assert(Lista4.isTreeFull(Lista4.createTree(0, 1, 10)))
    assert(Lista4.isTreeFull(Lista4.createTree(3, 1, 10)))
    assert(Lista4.isTreeFull(Lista4.createTree(4, 1, 10)))
  }

}
