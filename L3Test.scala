import org.scalatest.FunSuite

class L3Test extends FunSuite {
  val tested = new L3()

  // 1)
  test("testSplitTable") {
    assert(tested.splitTable(List(-3,-6,8,-9,13))==(List(-3,-6,-9),List(-3,-9)))
    assert(tested.splitTable(List(3,6,8,9,13,11,32,123,51))==(Nil,Nil))
    assert(tested.splitTable(List(-3,-7,-9,-13))==(List(-3,-7,-9,-13),List(-3,-7,-9,-13)))
    assert(tested.splitTable(List(-2,-2,-2,-2,-2))==(List(-2,-2,-2,-2,-2),Nil))
  }

  // 2)
  test("testLength") {
    assert(tested.length(List(-3,-6,8,-9,13,12,3,12,1,2))==10)
    assert(tested.length(List(123.231,"las",123,'s','a',123.1))==6)
    assert(tested.length(Nil)==0)
    assert(tested.length(List('a','a','a','a','a','a','a','a','a','a','a'))==11)
  }

  // 3)
  test("testMergeLists") {
    assert(tested.mergeLists(List('a','b','c','d','e','f'),List(1,2,3,4,5,6)) ==
      List('a',1,'b',2,'c',3,'d',4,'e',5,'f',6))
    assert(tested.mergeLists(List('a','b','c'),List(1,2,3,4,5,6)) == List('a',1,'b',2,'c',3,4,5,6))
    assert(tested.mergeLists(Nil,List(1,2,3,4,5,6)) == List(1,2,3,4,5,6))
    assert(tested.mergeLists(List(1,2,3,4,5,6),List()) == List(1,2,3,4,5,6))
    assert(tested.mergeLists(Nil,Nil) == Nil)
  }

  // 4)
  test("testStrContains") {
    assert(tested.strContains("as","as"))
    assert(tested.strContains("masło","as"))
    assert(!tested.strContains("12an45","am"))
    assert(tested.strContains("12123","123"))
    assert(tested.strContains("112123","123"))
  }

  test("testFindRec") {
    assert(tested.findRec(List("egazamin","torebka","kostium","szminka"),"ka")==List("torebka","szminka"))

    assert(tested.findRec(List("masło","las","kas","tak","nie","niewiem"),"as")==List("masło","las","kas"))

    assert(tested.findRec(List("5124123345","12353453456","3456346123","346372421","12444124125","1212121212",
      "456634312123312"),"123")
      ==List("5124123345","12353453456","3456346123","456634312123312"))

    assert(tested.findRec(List("asdasd","gadasdw","sadasda","sdgsdgsdf"),"")
      ==List("asdasd","gadasdw","sadasda","sdgsdgsdf"))

    assert(tested.findRec(Nil,"asdasd")==Nil)
  }

  test("testFindTailRec") {
    assert(tested.findTailRec(List("egazamin","torebka","kostium","szminka"),"ka")==List("torebka","szminka"))

    assert(tested.findTailRec(List("masło","las","kas","tak","nie","niewiem"),"as")==List("masło","las","kas"))

    assert(tested.findTailRec(List("5124123345","12353453456","3456346123","346372421","12444124125","1212121212",
      "456634312123312"),"123")
      ==List("5124123345","12353453456","3456346123","456634312123312"))

    assert(tested.findTailRec(List("asdasd","gadasdw","sadasda","sdgsdgsdf"),"")
      ==List("asdasd","gadasdw","sadasda","sdgsdgsdf"))

    assert(tested.findTailRec(Nil,"asdasd")==Nil)
  }

  test("testFindMultiRec") {
    assert(tested.findMultiRec(List("egazamin","torebka","kostium","szminka"),List("ka","m"))==
      List("egazamin","torebka","kostium","szminka"))

    assert(tested.findMultiRec(List("masło","las","kas","tak","nie","niewiem"),List("as","ie"))
      ==List("masło","las","kas","nie","niewiem"))

    assert(tested.findMultiRec(List("5124123345","12353453456","3456346123","346372421","12444124125","1212121212",
      "456634312123312","141241244412","1231231444122","122121241245151221"),
      List("123","444"))
      ==List("5124123345","12353453456","3456346123","12444124125","456634312123312","141241244412","1231231444122"))

    assert(tested.findMultiRec(List("asdasd","gadasdw","sadasda","sdgsdgsdf"),List("","",""))==
      List("asdasd","gadasdw","sadasda","sdgsdgsdf"))

    assert(tested.findMultiRec(Nil,List("asdasd","123123","1231231"))==Nil)

    assert(tested.findMultiRec(List("123123","132123"),Nil)==Nil)
  }

  test("testFindMultiTailRec") {
    assert(tested.findMultiTailRec(List("egazamin","torebka","kostium","szminka"),List("ka","m"))==
      List("egazamin","torebka","kostium","szminka"))

    assert(tested.findMultiTailRec(List("masło","las","kas","tak","nie","niewiem"),List("as","ie"))
      ==List("masło","las","kas","nie","niewiem"))

    assert(tested.findMultiTailRec(List("5124123345","12353453456","3456346123","346372421","12444124125","1212121212",
      "456634312123312","141241244412","1231231444122","122121241245151221"),
      List("123","444"))
      ==List("5124123345","12353453456","3456346123","12444124125","456634312123312","141241244412","1231231444122"))

    assert(tested.findMultiTailRec(List("asdasd","gadasdw","sadasda","sdgsdgsdf"),List("","",""))==
      List("asdasd","gadasdw","sadasda","sdgsdgsdf"))

    assert(tested.findMultiTailRec(Nil,List("asdasd","123123","1231231"))==Nil)

    assert(tested.findMultiTailRec(List("123123","132123"),Nil)==Nil)
  }

  // 5)
  test("testJoinListsRec") {
    assert(tested.joinListsRec(List(5,4,3,2),List("Asdasd",'a'),List(2.2312,23.12312,3.123123))==
      List(5,4,3,2,"Asdasd",'a',2.2312,23.12312,3.123123))

    assert(tested.joinListsRec(List(5,4,3,2),Nil,List(2.2312,23.12312,3.123123))==
      List(5,4,3,2,2.2312,23.12312,3.123123))

    assert(tested.joinListsRec(Nil,Nil,List(2.2312,23.12312,3.123123))== List(2.2312,23.12312,3.123123))

    assert(tested.joinListsRec(List(5,4,3,2),Nil,Nil)==List(5,4,3,2))

    assert(tested.joinListsRec(Nil,Nil,Nil)==Nil)
  }

  test("testJoinListsTailRec") {
    assert(tested.joinListsTailRec(List(5,4,3,2),List("Asdasd",'a'),List(2.2312,23.12312,3.123123))==
      List(5,4,3,2,"Asdasd",'a',2.2312,23.12312,3.123123))

    assert(tested.joinListsTailRec(List(5,4,3,2),Nil,List(2.2312,23.12312,3.123123))==
      List(5,4,3,2,2.2312,23.12312,3.123123))

    assert(tested.joinListsRec(Nil,Nil,List(2.2312,23.12312,3.123123))== List(2.2312,23.12312,3.123123))

    assert(tested.joinListsTailRec(List(5,4,3,2),Nil,Nil)==List(5,4,3,2))

    assert(tested.joinListsTailRec(Nil,Nil,Nil)==Nil)
  }
}
