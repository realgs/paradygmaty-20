import Functions.{concatLists, findEl, findElement, findElements, findEls, listLength, joinLists, joinListsTail, splitElements}

object Tests {
  def main(args: Array[String]): Unit = {
    // Task 1
    println(splitElements(List()) == (List(), List()))
    println(splitElements(List(1)) == (List(), List()))
    println(splitElements(List(-2)) == (List(-2), List()))
    println(splitElements(List(-3)) == (List(-3), List(-3)))
    println(splitElements(List(-3, -6, 8, -9, 13)) == (List(-3, -6, -9), List(-3, -9)))
    println(splitElements(List(-1, -1, -1, 2, 3, -3, -6, -5)) == (List(-1, -1, -1, -3, -6, -5), List(-1, -1, -1, -3, -5)))
    println(splitElements(List(0, 0, -3, -6, -5)) == (List(-3, -6, -5), List(-3, -5)))

    // Task 2
    println(listLength(List()) == 0)
    println(listLength(List(0)) == 1)
    println(listLength(List("test", "test")) == 2)
    println(listLength(List(-3, -6, 8, -9, 13)) == 5)

    // Task 3
    println(concatLists(List(), List()) == List())
    println(concatLists(List(1, 1, 1), List()) == List(1, 1, 1))
    println(concatLists(List(), List(3.13, 3.14, 3.15)) == List(3.13, 3.14, 3.15))
    println(concatLists(List(1, 3, 5, 7), List(2, 4, 6, 8)) == List(1, 2, 3, 4, 5, 6, 7, 8))
    println(concatLists(List("It", "work", "well"), List("does", "quite")) == List("It", "does", "work", "quite", "well"))

    // Task 4
    val stringList = List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224")

    println(findElement(Nil, "") == List())
    println(findElement(Nil, "index") == List())
    println(findElement(stringList, "68") == List("index0168202", "index0168211", "index0168210"))
    println(findElement(stringList, "index0169") == List("index0169", "index0169222", "index0169224"))
    println(findElement(stringList, "index016821") == List("index0168211", "index0168210"))
    println(findElement(stringList, "x") == stringList)

    println(findEl(Nil, "") == List())
    println(findEl(Nil, "index") == List())
    println(findEl(stringList, "68") == List("index0168202", "index0168211", "index0168210"))
    println(findEl(stringList, "index0169") == List("index0169", "index0169222", "index0169224"))
    println(findEl(stringList, "index016821") == List("index0168211", "index0168210"))
    println(findEl(stringList, "x") == stringList)

    println(findElement(stringList, "index016") == findEl(stringList, "index016"))
    println(findElement(stringList, "index0168") == findEl(stringList, "index0168"))
    println(findElement(stringList, "index016821") == findEl(stringList, "index016821"))
    println(findElement(stringList, "index0168202") == findEl(stringList, "index0168202"))

    println(findElements(stringList, List()) == List())
    println(findElements(stringList, List("index01692", "index01682")) == List("index0168202", "index0168211", "index0168210", "index0169222", "index0169224"))
    println(findElements(stringList, List("index01692", "index01682")) == findEls(stringList, List("index01692", "index01682")))
    println(findElements(stringList, List("index0169222", "index0168202")) == findEls(stringList, List("index0169222", "index0168202")))
    println(findElements(stringList, List("a", "b")) == List())
    println(findElements(stringList, List("8", "9")) == stringList)

    println(findEls(stringList, List()) == List())
    println(findEls(stringList, List("index01692", "index01682")) == List("index0168202", "index0168211", "index0168210", "index0169222", "index0169224"))
    println(findEls(stringList, List("index01692", "index01682")) == findEls(stringList, List("index01692", "index01682")))
    println(findEls(stringList, List("index0169222", "index0168202")) == findEls(stringList, List("index0169222", "index0168202")))
    println(findEls(stringList, List("a", "b")) == List())
    println(findEls(stringList, List("8", "9")) == stringList)

    val stringList2 = List("1", "111", "121", "123", "312", "321", "12", "22", "31", "33", "332", "4", "44", "41", "14", "43", "41231")
    println(findElements(stringList2, List("1", "2")) == List("1", "111", "121", "123", "312", "321", "12", "22", "31", "332", "41", "14", "41231"))
    println(findElements(stringList2, List("4", "5")) == List("4", "44", "41", "14", "43", "41231"))

    println(findEls(stringList2, List("1", "2")) == List("1", "111", "121", "123", "312", "321", "12", "22", "31", "332", "41", "14", "41231"))
    println(findEls(stringList2, List("4", "5")) == List("4", "44", "41", "14", "43", "41231"))

    // Task 5
    println(joinLists(List(), List(), List()) == List())
    println(joinLists(List(3, 2, 1), List(), List()) == List(3, 2, 1))
    println(joinLists(List(), List(3, 1, 2), List()) == List(3, 1, 2))
    println(joinLists(List(), List(), List(7, 8, 9, 2)) == List(7, 8, 9, 2))
    println(joinLists(List(), List(4,5,6), List(7, 8, 9, 2)) == List(4, 5, 6, 7, 8, 9, 2))
    println(joinLists(List(1,2,3), List(4,5,6), List(7, 8, 9)) == List(1, 2, 3, 4, 5, 6, 7, 8, 9))
    println(joinLists(List(1.11, 2.22, 3.33), List(4.44, 5.55, 6.66), List(7.77, 8.88, 9.99)) == List(1.11, 2.22, 3.33, 4.44, 5.55, 6.66, 7.77, 8.88, 9.99))
    println(joinLists(List("Is", "it"), List( "late", "already"), List("?")) == List("Is", "it", "late", "already", "?"))
    println(joinLists(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))

    println(joinListsTail(List(), List(), List()) == List())
    println(joinListsTail(List(3, 2, 1), List(), List()) == List(3, 2, 1))
    println(joinListsTail(List(), List(3, 1, 2), List()) == List(3, 1, 2))
    println(joinListsTail(List(), List(), List(7, 8, 9, 2)) == List(7, 8, 9, 2))
    println(joinListsTail(List(), List(4,5,6), List(7, 8, 9, 2)) == List(4, 5, 6, 7, 8, 9, 2))
    println(joinListsTail(List(1,2,3), List(4,5,6), List(7, 8, 9)) == List(1, 2, 3, 4, 5, 6, 7, 8, 9))
    println(joinListsTail(List(1.11, 2.22, 3.33), List(4.44, 5.55, 6.66), List(7.77, 8.88, 9.99)) == List(1.11, 2.22, 3.33, 4.44, 5.55, 6.66, 7.77, 8.88, 9.99))
    println(joinListsTail(List("Is", "it"), List( "late", "already"), List("?")) == List("Is", "it", "late", "already", "?"))
    println(joinListsTail(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
  }
}
