import L4._
object TestL4 {
  def main(args: Array[String]) {
    // Testy zadanie 1 3pkt.
    testyZadanie1()
    // Testy zadanie 2 3pkt.
    testyZadanie2()
    // Testy zadanie 3 4pkt.
    testyZadanie3()
    // Testy zadanie 4 5pkt.
    testyZadanie4()
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
  def testyZadanie3(): Unit ={
    val simpleTree = Node(1,Node(2,Empty,Empty),Node(3,Empty,Empty))
    val simpleTree2 = Node(1,Node(3,Empty,Empty),Node(3,Empty,Empty))
    val simpleTestResult = (Node(-1,Node(2,Empty,Empty),Empty),Node(-1,Node(3,Empty,Empty),Empty))

    val tree = Node(5,Node(3,Node(1,Empty,Empty),Node(2,Empty,Empty)),Node(4,Node(0,Empty,Empty),Node(2,Empty,Empty)))
    val tree2 = Node(10,Node(2,Node(1,Empty,Empty),Node(3,Empty,Empty)),Node(4,Node(0,Empty,Empty),Node(3,Empty,Empty)))

    val testResult = (Node(5,Node(3,Empty,Node(2,Empty,Empty)),Node(-1,Empty,Node(2,Empty,Empty))),
      Node(10,Node(2,Empty,Node(3,Empty,Empty)),Node(-1,Empty,Node(3,Empty,Empty))))

    println("Test 1 Zadanie 3: " + (eraseDuplicatesDepthSearch(Empty,Empty) == (Empty,Empty))+ " " +
      (extractTreesFromBreadthQueue(eraseDuplicatesBreadthSearch(Empty,Empty)) == (Empty,Empty)))
    println("Test 2 Zadanie 3: " + (eraseDuplicatesDepthSearch(simpleTree,simpleTree) == (Empty,Empty))+ " " +
      (extractTreesFromBreadthQueue(eraseDuplicatesBreadthSearch(simpleTree,simpleTree)) == (Empty,Empty)))
    println("Test 3 Zadanie 3: " + (eraseDuplicatesDepthSearch(simpleTree,simpleTree2) == simpleTestResult)+ " " +
      (extractTreesFromBreadthQueue(eraseDuplicatesBreadthSearch(simpleTree,simpleTree2)) == simpleTestResult))
    println("Test 4 Zadanie 3: " + (eraseDuplicatesDepthSearch(tree,tree2) == testResult)+ " " +
      (extractTreesFromBreadthQueue(eraseDuplicatesBreadthSearch(tree,tree2)) == testResult)+"\n")
  }
  def testyZadanie4(): Unit ={
    println("Test 1 zadanie 4: " + (eachNElement(LazyList(5,6,3,2,1),2,3).toList == List(5,3)))
    println("Test 2 zadanie 4: " + (eachNElement(LazyList(5,6,3,2,1),2,4).toList == List(5,3)))
    println("Test 3 zadanie 4: " + (eachNElement(LazyList("A","B","C","D","E","F"),1,3).toList == List("A","B","C")))
    println("Test 4 zadanie 4: " + (eachNElement(LazyList(1.2,2.4,3.1),0,3).toList == List(1.2)))
    println("Test 5 zadanie 4: " + (eachNElement(LazyList(1,2,3),3,0).toList == List()))
    println("Test 6 zadanie 4: " + (eachNElement(getInfiniteLazyList(2),2,10000).toList == getListOfEvenNumbers(2,10000)))
    println("Test 7 zadanie 4: " + (eachNElement(getInfiniteLazyList(1),2,10000).toList == getListOfOddNumbers(1,10000))+"\n")
  }
}

