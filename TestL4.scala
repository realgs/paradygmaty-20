import L4._
object TestL4 {
  def main(args: Array[String]) {
    // Testy zadanie 1 3pkt.
    testyZadanie1()
  }

  def testyZadanie1(): Unit ={
    println("Test 1 zadanie 1: " + breadthSearch(generateTree((1,10),-1)).isEmpty)
    println("Test 2 zadanie 1: " + breadthSearch(generateTree((1,10),0)).isEmpty)
    println("Test 3 zadanie 1: " + (breadthSearch(generateTree((0,0),3)) == List(0,0,0,0,0,0,0)))
    println("Test 4 zadanie 1: " + (breadthSearch(generateTree((1,10),10)).size == 1023)+"\n")
  }
}

