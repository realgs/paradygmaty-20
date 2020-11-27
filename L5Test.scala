import org.scalatest.FunSuite

import scala.collection.immutable.Queue

class Point(xv: Double, yv: Int) extends Debug {
  var x: Double = xv
  var y: Int = yv
  var a: Char = 'T'
}

class L5Test extends FunSuite {
  val tested = new L5
  val testedClass = new Point(3.123,4)

  test("testDuplicate") {
    val collection1 = Queue(1,2,4,6)
    val duplications1 = Queue(0,3,2,1)
    val expected1 = Queue(2,2,2,4,4,6)
    assert(tested.duplicate(collection1,duplications1) == expected1)

    val collection2 = Queue(1,2,4)
    val duplications2 = Queue(0,3,2,1)
    val expected2 = Queue(2,2,2,4,4)
    assert(tested.duplicate(collection2,duplications2) == expected2)

    val collection3 = Queue(1,2,4,6,7,10)
    val duplications3 = Queue(0,3,2,1)
    val expected3 = Queue(2,2,2,4,4,6)
    assert(tested.duplicate(collection3,duplications3) == expected3)

    val collection4 = Queue()
    val duplications4 = Queue(0,3,2,1)
    val expected4 = Queue()
    assert(tested.duplicate(collection4,duplications4) == expected4)

    val collection5 = Queue(1,2,4,6,7,10)
    val duplications5 = Queue()
    val expected5 = Queue()
    assert(tested.duplicate(collection5,duplications5) == expected5)
  }

  test("testDuplicateOnce") {
    val collection1 = Queue(1,2,4,6)
    val duplications1 = Queue(0,3,2,1)
    val expected1 = Queue(2,2,2,4,4,6)
    assert(tested.duplicateOnce(collection1,duplications1) == expected1)

    val collection2 = Queue(1,1,1,1,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4)
    val duplications2 = Queue(0,3,2,1)
    val expected2 = Queue(2,2,2,4,4)
    assert(tested.duplicateOnce(collection2,duplications2) == expected2)
  }

  test("testGetName") {
    assert(testedClass.getClassName == "Point")
  }

  test("testGetFields") {
    val array = testedClass.getFields

    val (n1,t1,v1) = array(0)
    assert(n1 == "x")
    assert(t1.toString == "double")
    assert(v1 == 3.123)

    val (n2,t2,v2) = array(1)
    assert(n2 == "y")
    assert(t2.toString == "int")
    assert(v2 == 4)

    val (n3,t3,v3) = array(2)
    assert(n3 == "a")
    assert(t3.toString == "char")
    assert(v3 == 'T')
  }

}
