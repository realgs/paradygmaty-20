import L4._
object TestL4 {
  def main(args: Array[String]) {
    // Testy zadanie 1 3pkt.
    testyZadanie1()
    // Testy zadanie 2 3pkt.
    testyZadanie2()
  }

  def testyZadanie1(): Unit ={
    println("Test 1 zadanie 1: " + breadthSearch(generateTree((1,10),-1)).isEmpty)
    println("Test 2 zadanie 1: " + breadthSearch(generateTree((1,10),0)).isEmpty)
    println("Test 3 zadanie 1: " + (breadthSearch(generateTree((0,0),3)) == List(0,0,0,0,0,0,0)))
    println("Test 4 zadanie 1: " + (breadthSearch(generateTree((1,10),10)).size == 1023)+"\n")
  }
  def testyZadanie2(): Unit ={
    val tree = Node(5,Node(3,Node(1,Empty,Empty),Node(2,Empty,Empty)),Node(4,Node(0,Empty,Empty),Node(2,Empty,Empty)))
    val tree2 = Node(10,Node(2,Node(1,Empty,Empty),Node(3,Empty,Empty)),Node(4,Node(0,Empty,Empty),Node(3,Empty,Empty)))

    val testResult = Node(-5,Node(1,Node(0,Empty,Empty),Node(-1,Empty,Empty)),Node(0,Node(0,Empty,Empty),Node(-1,Empty,Empty)))

    println("Test 1 zadanie 2: " + breadthSearch(generateTreeOfDifference(generateTree((1,10),-1),generateTree((1,10),-1))).isEmpty)
    println("Test 2 zadanie 2: " + breadthSearch(generateTreeOfDifference(generateTree((1,10),0),generateTree((1,10),0))).isEmpty)
    println("Test 3 zadanie 2: " + (breadthSearch(generateTreeOfDifference(generateTree((0,0),3),generateTree((0,0),3))) == List(0,0,0,0,0,0,0)))
    println("Test 4 zadanie 2: " + (breadthSearch(generateTreeOfDifference(generateTree((1,10),10),generateTree((1,10),10))).size == 1023))
    println("Test 5 zadanie 2: " + (generateTreeOfDifference(tree,tree2) == testResult) + "\n")
  }
}

