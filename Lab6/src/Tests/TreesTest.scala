package Tests

import main.Trees._

object TreesTest{

  def runTest(): Unit = {
    test1()
    test2()
    test3()
  }

  def test1(): Unit = {
    println("\n1.Tree test")
    val a = Node(3, Node(4, Node(2, Empty, Empty), Node(12, Empty, Empty)), Node(10, Node(2, Empty, Empty), Node(5, Empty, Empty)))
    val b = Node(1, Node(5, Node(2, Empty, Empty), Node(12, Empty, Empty)), Node(10, Node(1, Empty, Empty), Node(5, Empty, Empty)))
    val result1 = (Node(3, Node(4, Empty, Empty), Node(-1, Node(2, Empty, Empty), Empty)),
      Node(1, Node(5, Empty, Empty), Node(-1, Node(1, Empty, Empty), Empty)))
    println(removeRepetition(a, b) == result1)
    println(removeRepetitionParallel(a, b) == result1)
    println(removeRepetitionFuture(a, b) == result1)
  }

  def test2(): Unit = {
    println("\n2.Tree test")
    val c = Node(3, Node(4, Empty, Empty), Node(10, Empty, Empty))
    val d = Node(1, Node(4, Empty, Empty), Node(10, Empty, Empty))
    val result2 = (Node(3, Empty, Empty), Node(1, Empty, Empty))
    println(removeRepetition(c, d) == result2)
    println(removeRepetitionFuture(c, d) == result2)
    println(removeRepetitionParallel(c, d) == result2)
  }

  def test3(): Unit = {
    println("\n3.Tree test")
    val e = Empty
    val f = Empty
    println(removeRepetition(e, f) == (Empty, Empty))
    println(removeRepetitionFuture(e, f) == (Empty, Empty))
    println(removeRepetitionParallel(e, f) == (Empty, Empty))

  }

}
