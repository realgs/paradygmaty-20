package Tests


import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._
import Source.Ex1_2.duplicateNoRepetitions
import scala.collection.{mutable => m}


class Ex2Test {

  @Test def testRegularRepetitions(): Unit =
  {
    val collection = m.LinkedHashSet().addAll(List(1, 2, 3, 4))
    val numRepeats = m.Queue().addAll(List(4, 3, 2, 1, 100))
    assertEquals(m.Queue().addAll(List(1, 1, 1, 1, 2, 2, 2, 3, 3, 4)), duplicateNoRepetitions(collection, numRepeats))
  }


  @Test def testEmptyCollection(): Unit =
  {
    val collection = m.LinkedHashSet[Int]()
    val numRepeats = m.Queue().addAll(List(1, 2))
    assertEquals(m.Queue[Int](), duplicateNoRepetitions(collection, numRepeats))
  }

  @Test def testZerosInNumRepeats(): Unit =
  {
    val collection = m.LinkedHashSet().addAll(List('A', 'B', 'C'))
    val numRepeats = m.Queue().addAll(List(0, 2, 1, 1))
    assertEquals(m.Queue().addAll(List('B', 'B', 'C')), duplicateNoRepetitions(collection, numRepeats))
  }

  @Test def testNegativesInNumRepeats(): Unit =
  {
    val collection = m.LinkedHashSet().addAll(List('*', '-', '^'))
    val numRepeats = m.Queue().addAll(List(-4, -1, 0))
    assertEquals(m.Queue[Char](), duplicateNoRepetitions(collection, numRepeats))
  }

  @Test def testCollectionGreaterThanRepeats(): Unit =
  {
    val collection = m.LinkedHashSet().addAll(List("Ala", "ma", "psa"))
    val numRepeats = m.Queue().addAll(List(1, 2))
    assertThrows(classOf[Exception], () => duplicateNoRepetitions(collection, numRepeats))
  }

  @Test def testCollectionTryAddRepetitions(): Unit =
  {
    val collection = m.LinkedHashSet().addAll(List("Ala", "ma", "psa", "ma"))
    val numRepeats = m.Queue().addAll(List(1, 2, 1, 10, 0, 0))
    assertEquals(m.Queue().addAll(List("Ala", "ma", "ma", "psa")), duplicateNoRepetitions(collection, numRepeats))
  }
}
