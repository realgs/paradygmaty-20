import org.scalatest.FunSuite

class L2Test extends FunSuite {
  val tested = new L2()
  
  test("productOfList Test"){
    assert(tested.productOfList(List(2,4,5,2)) == 80)
    assert(tested.productOfList(List()) == 0)
    assert(tested.productOfList(List(-3,-1,-1,12,3,2)) == -216)
    assert(tested.productOfList(List(-1)) == -1)
    assert(tested.productOfList(List(1,5,2,5,-1)) == -50)
  }

  test("makeSentence Test"){
    assert(tested.makeSentence(List("To","be", "or", "not","to","be"), ' ', '.') == "To be or not to be.")
    assert(tested.makeSentence(List("a","b","c","d","e","f"), ',', '.') == "a,b,c,d,e,f.")
    assert(tested.makeSentence(List("1","2","3","4","5"), '+', '=') == "1+2+3+4+5=")
    assert(tested.makeSentence(List(), ' ', '.') == "")
    assert(tested.makeSentence(List("Hello"), ' ', '!') == "Hello!")
  }

  test("interval Test"){
    assert(tested.interval(List(-1,0,2), -2, 4) == true)
    assert(tested.interval(List(-1,0,2), 4, -2) == true)
    assert(tested.interval(List(1,3,4,5), 1, 3) == false)
    assert(tested.interval(List(5,5,5,5), 5, 5) == true)
    assert(tested.interval(List(-5.2, -6.1, 5,4.3,-2.3), -10, 15.5) == true)

  }

  test("power Test"){
    assert(tested.power(3,2) == 9)
    assert(tested.power(0, 2) == 0)
    assert(tested.power(10,0) == 1)
    assert((math floor tested.power(-4.6, 5)*100000)/100000 == -2059.62976 )
    assert(tested.power(2, -4) == 0.0625)
  }
}
