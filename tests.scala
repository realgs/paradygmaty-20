import org.junit.jupiter.api.{Assertions, BeforeAll, Test}

class tests {
  val testList = List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224")
  val testFunction = new Functions

  @Test def testFunction1: Unit = {
    assert(testFunction.split(List(-3, -6, 8, -9, 13)) == (List(-3, -6, -9), List(-3, -9)))
    assert(testFunction.split(List()) == (List(), List()))
    assert(testFunction.split(List(-1, -2, -3, -4, -5, -6, -7, 1, 2, 3)) == (List(-1, -2, -3, -4, -5, -6, -7), List(-1, -3, -5, -7)))
    assert(testFunction.split(List(1, 2, 3)) == (List(), List()))
    assert(testFunction.split(List(-1)) == (List(-1), List(-1)))
  }

  @Test def testFunction2: Unit = {
    assert(testFunction.length(List(5, 4, 3, 2)) == 4)
    assert(testFunction.length(List()) == 0)
    assert(testFunction.length(List('k', 'a', 'm', 'i', 'l', 's', 'l', 'i', 'm', 'a', 'k')) == 11)
    assert(testFunction.length(List("elo")) == 1)
  }

  @Test def testFunction3: Unit = {
    assert(testFunction.append(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6))
    assert(testFunction.append(List(), List()) == List())
    assert(testFunction.append(List(1, 2, 3), List()) == List(1, 2, 3))
    assert(testFunction.append(List("ala", "kota", "pieska"), List("ma", "i")) == List("ala", "ma", "kota", "i", "pieska"))
  }

  @Test def testFunction4: Unit = {
    //one pattern rec function
    assert(testFunction.find(testList, "x") == testList)
    assert(testFunction.find(Nil, "halko") == List())
    assert(testFunction.find(testList, "168") == List("index0168202", "index0168211", "index0168210"))
    assert(testFunction.find(testList, "indx") == List())
    print(testFunction.find(testList, "index0168202") == List("index0168202"))

    //one pattern tailrec function
    assert(testFunction.findTail(testList, "x") == testList)
    assert(testFunction.findTail(Nil, "halko") == List())
    assert(testFunction.findTail(testList, "168") == List("index0168202", "index0168211", "index0168210"))
    assert(testFunction.findTail(testList, "indx") == List())
    print(testFunction.findTail(testList, "index0168202") == List("index0168202"))

    //N patterns rec function
    assert(testFunction.findN(testList, List("21", "68")) == List("index0168202", "index0168211", "index0168210"))
    assert(testFunction.findN(testList, List("abba", "53")) == List())

    //N patterns recTail function
    assert(testFunction.findNTail(testList, List("21", "68")) == List("index0168202", "index0168211", "index0168210"))
    assert(testFunction.findNTail(testList, List("abba", "53")) == List())
  }
}
