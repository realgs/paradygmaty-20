import lazyLists._

object lazyListsTest {

  def zad4Test(): Unit = {
    println("List of nth elements test")
    println(EachNElement(LazyList(5, 6, 3, 2, 1), 2, 3) == LazyList(5, 3))
    println(EachNElement(LazyList('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'), 2, 4) == LazyList('a', 'c'))
    println(EachNElement(LazyList(1, 2, 3, 4, 5), 50, 2) == LazyList(1))
    println(EachNElement(LazyList(1, 2, 3, 4, 5), 1, 5) == LazyList(1, 2, 3, 4, 5))
    println(EachNElement(LazyList(1, 2, 3, 4, 5), 2, 50) == LazyList(1, 3, 5))
    println(EachNElement(LazyList(), 1, 50) == LazyList())
  }

  def zad5Test(): Unit = {
    println("Operation on numbers test")
    println(ldzialanie(LazyList(1, 2), LazyList(), +) == LazyList(1, 2))
    println(ldzialanie(LazyList(), LazyList(3.14, -1.2), -) == LazyList(3.14, -1.2))
    println(ldzialanie(LazyList(1.999, 2.2, 3, 4, 5), LazyList(5, 4, 3, 2.2, 1.999), *) == LazyList(9.995, 8.8, 9, 8.8, 9.995))
    println(ldzialanie(LazyList(1, 2, 3, 4, 5), LazyList(5, 4, 3, 2), /) == LazyList(0.2, 0.5, 1, 2, 5))
    println(ldzialanie(LazyList(), LazyList(), /) == LazyList())
  }

}

