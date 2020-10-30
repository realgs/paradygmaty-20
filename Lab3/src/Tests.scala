import Functions._

object Tests {

  def main(args:Array[String]):Unit = {

    // Function 1. tests
    println(divide(List(-3, -6, 8, -9, 13)) == (List(-3, -6, -9), List(-3, -9)))
    println(divide(List(1, 2, 3, 4, 5, 6, 7)) == (Nil, Nil))
    println(divide(List(0, 2, -2, -4, -6, 3, 4, 5, -10)) == (List(-2, -4, -6, -10), Nil))
    println(divide(List(-3, -3, -3, -3)) == (List(-3, -3, -3, -3), List(-3, -3, -3, -3)))
    println(divide(Nil) == (Nil, Nil))

    // Function 2. tests
    println(length(List(5, 4, 3, 2)) == 4)
    println(length(List(1, 1, 1)) == 3)
    println(length(List(-10000.32, 98, 0, 0, 1232.9, 12)) == 6)
    println(length(List("aaa")) == 1)
    println(length(List("aaa", "b", "cd", "asd", "qwerty")) == 5)
    println(length(Nil) == 0)

    // Function 3. tests
    println(append(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6))
    println(append(List(1, 2, 3, 4, 5, 6), List(5, 4, 3, 2)) == List(1, 5, 2, 4, 3, 3, 4, 2, 5, 6))
    println(append(Nil, List(1, 2, 3)) == List(1, 2, 3))
    println(append(List(-4214.9812, 0, 12321.32, 23), List(0, 0, 123.321, 0)) == List(-4214.9812, 0, 0, 0, 12321.32, 123.321, 23, 0))
    println(append(List("a", "c", "e", "f"), List("b", "d")) == List("a", "b", "c", "d", "e", "f"))
    println(append(Nil, Nil) == Nil)

    // Function 4. tests
    println(find(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), List("index0168")) == List("index0168202", "index0168211", "index0168210"))
    println(find(List("AAAAABAAAA", "BBBBBBABBB", "AB", "B", "A", "BAB", "ABAB", "AAAA", "BBBB"), List("AB")) == List("AAAAABAAAA", "BBBBBBABBB", "AB", "BAB", "ABAB"))
    println(find(List("AAAAABAAAA", "BBBBBBABBB", "AB", "B", "A", "BAB", "ABAB", "AAAA", "BBBB"), List("")) == List("AAAAABAAAA", "BBBBBBABBB", "AB", "B", "A", "BAB", "ABAB", "AAAA", "BBBB"))
    println(find(List("AAAAABAAAA", "BBBBBBABBB", "AB", "B", "A", "BAB", "ABAB", "AAAA", "BBBB"), List("CDCD")) == Nil)
    println(find(List("AAAAABAAAA", "BBBBBBABBB", "AB", "B", "A", "BAB", "ABAB", "AAAA", "BBBB"), List("AAA")) == List("AAAAABAAAA", "AAAA"))
    println(find(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), List("index0168", "index")))
    println(find(List("AAAAABAAAA", "BBBBBBABBB", "AB", "B", "A", "BAB", "ABAB", "AAAA", "BBBB"), List("AAA", "BBB")))
    println(find(Nil, List("", "a")) == Nil)
    println(find(Nil, Nil) == Nil)

    // Function 5. tests
    println(joinLists(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
    println(joinLists(List(5, 4, 3, 2), List(1, 0), Nil) == List(5, 4, 3, 2, 1, 0))
    println(joinLists(List(5, 4, 3, 2), Nil, List(9)) == List(5, 4, 3, 2, 9))
    println(joinLists(Nil, List(1, 0), Nil) == List(1, 0))
    println(joinLists(List(5, 4, 3, 2), Nil, Nil) == List(5, 4, 3, 2))
    println(joinLists(Nil, List(1, 0), List(9)) == List(1, 0, 9))
    println(joinLists(Nil, Nil, List(9)) == List(9))
    println(joinLists(Nil, Nil, Nil) == Nil)
    println(joinLists(List("a"), List("b", "c", "d"), List("e", "f")) == List("a", "b", "c", "d", "e", "f"))
    println(joinLists(List("a"), List("b", "b", "b", "b", "b", "b", "b", "b"), List("c")) == List("a", "b", "b", "b", "b", "b", "b", "b", "b", "c"))
  }

}
