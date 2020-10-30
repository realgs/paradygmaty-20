object Tests {

  def main(args: Array[String]): Unit = {
    task1Test()
    task2Test()
    task3Test()
    task4Test()
    task5Test()
  }

  def task1Test(): Unit = {
    println("Task 1 test:")
    println(Functions.split(List()) == (List(), List()))
    println(Functions.split(List(1, 2, 3, 4)) == (List(), List()))
    println(Functions.split(List(-1, -2, -3, -4, -5)) == (List(-1, -2, -3, -4, -5), List(-1, -3, -5)))
    println(Functions.split(List(0, 1, -2, -3, 0, 2)) == (List(-2, -3), List(-3)))
    println(Functions.split(List(-1)) == (List(-1), List(-1)))
  }

  def task2Test(): Unit = {
    println("Task 2 test:")
    println(Functions.length(List()) == 0)
    println(Functions.length(List(1.5, 0.11, 8.7, 10.5, 1.2, 0.6, 55.7)) == 7)
    println(Functions.length(List('a', 'b', 'c', 'd')) == 4)
    println(Functions.length(List("Jacek", "oddaj", "70", "mln")) == 4)
    println(Functions.length(List(1, 2, 3, 4, 5)) == 5)
  }

  def task3Test(): Unit = {
    println("Task 3 test:")
    println(Functions.merge(List(), List()) == List())
    println(Functions.merge(List(1, 2, 3, 4), List(-1, -2, -3, -4, -5, -6)) == List(1, -1, 2, -2, 3, -3, 4, -4, -5, -6))
    println(Functions.merge(List("ala", "kota"), List("ma")) == List("ala", "ma", "kota"))
    println(Functions.merge(List(1.2, 3.4, 5.6), List()) == List(1.2, 3.4, 5.6))
    println(Functions.merge(List(), List(11, 1000, 2)) == List(11, 1000, 2))
  }

  def task4Test(): Unit = {
    println("Task 4 test:\nWithout tail rec")
    println(Functions.find(List(), "") == List())
    println(Functions.find(List("aaaa", "bbbb", "c"), "") == List("aaaa", "bbbb", "c"))
    println(Functions.find(List(), "a") == List())
    println(Functions.find(List("malarz", "piosenkarz", "aktor", "arzyyyyy"), "arz") == List("malarz", "piosenkarz", "arzyyyyy"))
    println(Functions.find(List("aaab", "bababab", "bbbbbb", "abbbbbba"), "ba") == List("bababab", "abbbbbba"))

    println("With tail rec")
    println(Functions.find_Tail(List(), "") == List())
    println(Functions.find_Tail(List("aaaa", "bbbb", "c"), "") == List("aaaa", "bbbb", "c"))
    println(Functions.find_Tail(List(), "a") == List())
    println(Functions.find_Tail(List("malarz", "piosenkarz", "aktor", "arzyyyyy"), "arz") == List("malarz", "piosenkarz", "arzyyyyy"))
    println(Functions.find_Tail(List("aaab", "bababab", "bbbbbb", "abbbbbba"), "ba") == List("bababab", "abbbbbba"))

    println("For list of elements, without tail rec")
    println(Functions.findEach(List(), List()) == List())
    println(Functions.findEach(List("aaaa", "bbbb", "c"), List("", "")) == List("aaaa", "bbbb", "c"))
    println(Functions.findEach(List(), List("a", "b")) == List())
    println(Functions.findEach(List("malarz", "piosenkarz", "aktor", "arzyyyyy", "tokarz"), List("arz", "to")) == List("malarz", "piosenkarz", "arzyyyyy", "tokarz", "aktor"))
    println(Functions.findEach(List("aaab", "bababab", "bbbbbb", "abbbbbba"), List("ba", "ab")) == List("bababab", "abbbbbba", "aaab"))

    println("For list of elements, with tail rec")
    println(Functions.findEach_Tail(List(), List()) == List())
    println(Functions.findEach_Tail(List("aaaa", "bbbb", "c"), List("", "")) == List("aaaa", "bbbb", "c"))
    println(Functions.findEach_Tail(List(), List("a", "b")) == List())
    println(Functions.findEach_Tail(List("malarz", "piosenkarz", "aktor", "arzyyyyy", "tokarz"), List("arz", "to")) == List("malarz", "piosenkarz", "arzyyyyy", "tokarz", "aktor"))
    println(Functions.findEach_Tail(List("aaab", "bababab", "bbbbbb", "abbbbbba"), List("ba", "ab")) == List("bababab", "abbbbbba", "aaab"))
  }

  def task5Test(): Unit = {
    println("Task 5 test:\nWithout tail rec")
    println(Functions.joinLists(List(), List(), List()) == List())
    println(Functions.joinLists(List(1, 2), List(), List(3, 4)) == List(1, 2, 3, 4))
    println(Functions.joinLists(List(), List(1, 2), List()) == List(1, 2))
    println(Functions.joinLists(List(), List(), List(1, 2)) == List(1, 2))
    println(Functions.joinLists(List(1, 2), List(), List()) == List(1, 2))
    println(Functions.joinLists(List("rzad", "dziala"), List("gorzej", "niz"), List("JSOS")) == List("rzad", "dziala", "gorzej", "niz", "JSOS"))
    println(Functions.joinLists(List(-1.4, 1.7, 1.9), List(2.11, -2.7, 22.5), List(300.4, 3.9, 3.1)) == List(-1.4, 1.7, 1.9, 2.11, -2.7, 22.5, 300.4, 3.9, 3.1))

    println("With tail rec")
    println(Functions.joinLists_TailRec(List(), List(), List()) == List())
    println(Functions.joinLists_TailRec(List(1, 2), List(), List(3, 4)) == List(1, 2, 3, 4))
    println(Functions.joinLists_TailRec(List(), List(1, 2), List()) == List(1, 2))
    println(Functions.joinLists_TailRec(List(), List(), List(1, 2)) == List(1, 2))
    println(Functions.joinLists_TailRec(List(1, 2), List(), List()) == List(1, 2))
    println(Functions.joinLists_TailRec(List("rzad", "dziala"), List("gorzej", "niz"), List("JSOS")) == List("rzad", "dziala", "gorzej", "niz", "JSOS"))
    println(Functions.joinLists_TailRec(List(-1.4, 1.7, 1.9), List(2.11, -2.7, 22.5), List(300.4, 3.9, 3.1)) == List(-1.4, 1.7, 1.9, 2.11, -2.7, 22.5, 300.4, 3.9, 3.1))
  }
}
