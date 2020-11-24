package Lista5

import org.scalatest.FunSuite

import scala.collection.immutable.Queue

class Lista5Test extends FunSuite{
  test("Lista 5 zadanie 1") {
    assert(Lista5.duplicate(Queue(1,2,3), Queue(0,3,1,4)) == Queue(2,2,2,3))
    assert(Lista5.duplicate(Queue(1,2,3), Queue(1,4)) == Queue(1,2,2,2,2))
    assert(Lista5.duplicate(Queue('a','l','a'), Queue(1,2,-1)) == Queue('a','l','l'))
    assert(Lista5.duplicate(Queue(), Queue(0,3,1,4)) == Queue())
    assert(Lista5.duplicate(Queue(1,2,3), Queue()) == Queue())
  }

}
