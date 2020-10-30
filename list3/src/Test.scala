object Test {

  def zad1Test(): Unit = {
    println("Dividing into 2 lists - test")
    println("1." + (L3.podziel(List(-3, -4, 1, 9)) == (List(-3, -4), List(-3))).toString)
    println("2." + (L3.podziel(List(-3, -3, -1)) == (List(-3, -3, -1), List(-3, -3, -1))).toString)
    println("3." + (L3.podziel(List()) == (List(), List())).toString)
    println("4." + (L3.podziel(List(-3)) == (List(-3), List(-3))).toString)
    println("5." + (L3.podziel(List(4)) == (List(), List())).toString)
  }

  def zad2Test(): Unit = {
    println("\nFinding list length - test")
    println("1." + (L3.dlugosc(List('a', 'b', 'c')) == 3).toString)
    println("2." + (L3.dlugosc(List(1, 2, 3, 4, 5)) == 5).toString)
    println("3." + (L3.dlugosc(List()) == 0).toString)
    println("4." + (L3.dlugosc(List("ala")) == 1).toString)
    println("5." + (L3.dlugosc(List(-1, -1, -1)) == 3).toString)
  }

  def zad3Test(): Unit = {
    println("\nCombining 2 lists into one interlaced - test")
    println("1." + (L3.polacz(List("Ania", "malego"), List("ma", "kotka")) == List("Ania", "ma", "malego", "kotka")).toString)
    println("2." + (L3.polacz(List("Ania", "malego"), List()) == List("Ania", "malego")).toString)
    println("3." + (L3.polacz(List(), List("ma", "kotka")) == List("ma", "kotka")).toString)
    println("4." + (L3.polacz(List(1, 3, 5, 7, 9), List(2, 4, 6, 8)) == List(1, 2, 3, 4, 5, 6, 7, 8, 9)).toString)
    println("5." + (L3.polacz(List(), List()) == List()).toString)
  }

  def zad4Test(): Unit = {
    println("\nFinding list of patterns in list of texts - test")
    println("Finding with tailrec")
    println("1." + (L3.findTail(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), List("index0168")) == List("index0168202", "index0168211", "index0168210")).toString)
    println("2." + (L3.findTail(List("abaacb", "ccccc", "cacab", "aaabc"), List("da", "ac")) == List("abaacb", "cacab")).toString)
    println("3." + (L3.findTail(List(), List()) == List()).toString)
    println("4." + (L3.findTail(List("a"), List()) == List()).toString)
    println("5." + (L3.findTail(List(), List("a")) == List()).toString)
    println("6." + (L3.findTail(List("ala ma kota", "ten kot jest ali", "ona na na imie ala", "wysoka fala"), List("ala")) == List("ala ma kota", "ona na na imie ala", "wysoka fala")).toString)

    println("Finding without tailrec")
    println("1." + (L3.find(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), List("index0168")) == List("index0168202", "index0168211", "index0168210")).toString)
    println("2." + (L3.find(List("abaacb", "ccccc", "cacab", "aaabc", "aaaaada"), List("da", "ac")) == List("abaacb", "cacab", "aaaaada")).toString)
    println("3." + (L3.find(List(), List()) == List()).toString)
    println("4." + (L3.find(List("a"), List()) == List()).toString)
    println("5." + (L3.find(List(), List("a")) == List()).toString)

  }

  def zad5Test(): Unit = {
    println("\nCombining 3 lists - test")
    println("Combining with tailrec")
    println("1." + (L3.jointListsTail(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)) == List(1, 2, 3, 4, 5, 6, 7, 8, 9)).toString)
    println("2." + (L3.jointListsTail(List(), List(4, 5, 6), List(7, 8, 9)) == List(4, 5, 6, 7, 8, 9)).toString)
    println("3." + (L3.jointListsTail(List(1, 2, 3), List(), List(7, 8, 9)) == List(1, 2, 3, 7, 8, 9)).toString)
    println("4." + (L3.jointListsTail(List(1, 2, 3), List(4, 5, 6), List()) == List(1, 2, 3, 4, 5, 6)).toString)
    println("5." + (L3.jointListsTail(List(1, 2, 3), List(), List()) == List(1, 2, 3)).toString)
    println("6." + (L3.jointListsTail(List(), List(4, 5, 6), List()) == List(4, 5, 6)).toString)
    println("7." + (L3.jointListsTail(List(), List(), List(7, 8, 9)) == List(7, 8, 9)).toString)
    println("8." + (L3.jointListsTail(List(), List(), List()) == List()).toString)
    println("9." + (L3.jointListsTail(List("a"), List("b"), List("c")) == List("a", "b", "c")).toString)
    println("Combining without tailrec")
    println("1." + (L3.jointLists(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)) == List(1, 2, 3, 4, 5, 6, 7, 8, 9)).toString)
    println("2." + (L3.jointLists(List(), List(4, 5, 6), List(7, 8, 9)) == List(4, 5, 6, 7, 8, 9)).toString)
    println("3." + (L3.jointLists(List(1, 2, 3), List(), List(7, 8, 9)) == List(1, 2, 3, 7, 8, 9)).toString)
    println("4." + (L3.jointLists(List(1, 2, 3), List(4, 5, 6), List()) == List(1, 2, 3, 4, 5, 6)).toString)
    println("5." + (L3.jointLists(List(1, 2, 3), List(), List()) == List(1, 2, 3)).toString)
    println("6." + (L3.jointLists(List(), List(4, 5, 6), List()) == List(4, 5, 6)).toString)
    println("7." + (L3.jointLists(List(), List(), List(7, 8, 9)) == List(7, 8, 9)).toString)
    println("8." + (L3.jointLists(List(), List(), List()) == List()).toString)
    println("9." + (L3.jointLists(List("a"), List("b"), List("c")) == List("a", "b", "c")).toString)
  }
}

