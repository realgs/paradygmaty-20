package Tests

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._
import scala.collection.{mutable => m}

import Source.Ex1_2.duplicate


class Ex1Test {

  @Test def testRegularReplication(): Unit =
  {
    val collection = m.Queue().addAll(List(1, 2, 3, 4))
    val numRepeats = m.Queue().addAll(List(4, 3, 2, 1, 100))
    assertEquals(m.Queue().addAll(List(1, 1, 1, 1, 2, 2, 2, 3, 3, 4)), duplicate(collection, numRepeats))
  }

  @Test def testEmptyCollection(): Unit =
  {
    val collection = m.Queue[Int]()
    val numRepeats = m.Queue().addAll(List(1, 2))
    assertEquals(m.Queue[Int](), duplicate(collection, numRepeats))
  }

  @Test def testZerosInNumRepeats(): Unit =
  {
    val collection = m.Queue().addAll(List('A', 'B', 'C'))
    val numRepeats = m.Queue().addAll(List(0, 2, 1, 1))
    assertEquals(m.Queue().addAll(List('B', 'B', 'C')), duplicate(collection, numRepeats))
  }

  @Test def testNegativesInNumRepeats(): Unit =
  {
    val collection = m.Queue().addAll(List('*', '-', '^'))
    val numRepeats = m.Queue().addAll(List(-4, -1, 0))
    assertEquals(m.Queue[Char](), duplicate(collection, numRepeats))
  }

  @Test def testCollectionGreaterThanRepeats(): Unit =
  {
    val collection = m.Queue().addAll(List("Ala", "ma", "psa"))
    val numRepeats = m.Queue().addAll(List(1, 2))
    assertThrows(classOf[Exception], () => duplicate(collection, numRepeats))
  }
}
