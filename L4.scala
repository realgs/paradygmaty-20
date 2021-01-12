import scala.util.Random
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](value:A, left:BT[A], right:BT[A]) extends BT[A]

object L4 {
  // Zad 1 3pkt.
  def generateTree(boundaries:(Int,Int),heightOfTree:Int):BT[Int]={
    def helper(leftLevels:Int):BT[Int]={
      if(leftLevels == 1) Node(Random.nextInt(boundaries._2 - boundaries._1 + 1) + boundaries._1,Empty,Empty)
      else Node(Random.nextInt(boundaries._2 - boundaries._1 + 1) + boundaries._1,helper(leftLevels - 1),helper(leftLevels - 1))
    }
    if(heightOfTree <= 0) Empty
    else helper(heightOfTree)
  }

  def breadthSearch[A](tree:BT[A]):List[A]={
    def helper(queue:List[BT[A]]):List[A]={
      queue match {
        case Nil => Nil
        case Empty::tail => helper(tail)
        case Node(value,left,right)::tail =>  value::helper(tail:::List(left,right))
      }
    }
    helper(List(tree))
  }

  // Zad 2 3pkt.
  def generateTreeOfDifference(firstTree:BT[Int],secondTree:BT[Int]):BT[Int]={
    def helper(fTree:BT[Int],sTree:BT[Int]):BT[Int]={
      (fTree,sTree) match{
        case (Empty,Empty) => Empty
        case (Empty,_) | (_,Empty) => throw new Exception("Drzewa nie są sobie równe")
        case (Node(v1,l1,r1),Node(v2,l2,r2)) => Node(v1-v2,helper(l1,l2),helper(r1,r2))
      }
    }
    helper(firstTree,secondTree)
  }

  // Zad 3 4pkt.
  // Podpunkt A 1 pkt.
  def eraseDuplicatesDepthSearch(firstTree:BT[Int],secondTree:BT[Int]):(BT[Int],BT[Int])={
    def eraseDuplicates(fTree:BT[Int],sTree:BT[Int]):BT[Int]={
      (fTree,sTree) match{
        case (Empty,Empty) => Empty
        case (Empty,_) | (_,Empty) => throw new Exception("Drzewa nie są tej samej wielkosci!!!")
        case (Node(v1,l1,r1),Node(v2,l2,r2)) => {
          val leftSubTree = eraseDuplicates(l1,l2)
          val rightSubTree = eraseDuplicates(r1,r2)

          if(leftSubTree == Empty && rightSubTree == Empty){if(v1==v2)Empty else Node(v1,Empty,Empty)}
          else if(v1 == v2) Node(-1,leftSubTree,rightSubTree)
          else Node(v1,leftSubTree,rightSubTree)
        }
      }
    }
    (eraseDuplicates(firstTree,secondTree),eraseDuplicates(secondTree,firstTree))
  }

  // Podpunkt B 3pkt.
  def eraseDuplicatesBreadthSearch(firstTree:BT[Int],secondTree:BT[Int]):(Array[(Boolean,Int)],Array[(Boolean,Int)])={
    def search(queueFirst:List[BT[Int]],queueSecond:List[BT[Int]]):Array[(Boolean,Int)]={
      (queueFirst,queueSecond) match{
        case (Nil,Nil) => Array()
        case (Empty::t1,Empty::t2) => search(t1,t2)
        case (Node(v1,l1,r1)::t1,Node(v2,l2,r2)::t2) => (v1==v2,v1)+:search(t1++List(l1,r1),t2++List(l2,r2))
        case _ => throw new Exception("Drzewa nie sa tej samej wielkosci!!!")
      }
    }
    (search(List(firstTree),List(secondTree)),search(List(secondTree),List(firstTree)))
  }
  def extractTreesFromBreadthQueue(breadthSearchQueues:(Array[(Boolean,Int)],Array[(Boolean,Int)])):(BT[Int],BT[Int]) = {
    def helper(queue:Array[(Boolean,Int)],currentIndex:Int):BT[Int]={
      if(currentIndex - 1 >= queue.length) Empty // Wyjście poza drzewo
      else if(2 * currentIndex - 1 >= queue.length && 2 * currentIndex - 2 >= queue.length){ // Liść
        if(queue(currentIndex-1)._1) Empty
        else Node(queue(currentIndex-1)._2,Empty,Empty)
      }
      else{
        val leftSubTree = helper(queue,2*currentIndex)
        val rightSubTree = helper(queue,2*currentIndex+1)

        if(leftSubTree == Empty && rightSubTree == Empty){if(queue(currentIndex-1)._1)Empty else Node(queue(currentIndex-1)._2,Empty,Empty)}
        else if(queue(currentIndex-1)._1) Node(-1,leftSubTree,rightSubTree)
        else Node(queue(currentIndex-1)._2,leftSubTree,rightSubTree)
      }

    }
    (helper(breadthSearchQueues._1,1),helper(breadthSearchQueues._2,1))
  }

  // Zad 4 5pkt.
  def eachNElement[A](list:LazyList[A],n:Int,m:Int):LazyList[A]={
    def helper(list:LazyList[A],i:Int,m:Int):LazyList[A]={
      (n==i,m) match{
        case (true,0) => list.head#::LazyList()
        case (false,0) => LazyList()
        case (true,_) => list.head#::helper(list.tail,1,m-1)
        case (false,_) => helper(list.tail,i+1,m-1)
      }
    }
    if(m <= 0) LazyList()
    else helper(list,n,m-1)
  }

  // Nieskończona lista liczb całkowitych od 1
  def getInfiniteLazyList(element:Int):LazyList[Int] = element#::getInfiniteLazyList(element + 1)

  // Lista liczba parzystych od element do maksElement włącznie
  def getListOfEvenNumbers(element:Int,maksElement:Int):List[Int] = {
    if(element > maksElement) Nil
    else if(element % 2 == 0) element :: getListOfEvenNumbers(element + 1,maksElement)
    else getListOfEvenNumbers(element + 1,maksElement)
  }

  def getListOfOddNumbers(element:Int,maksElement:Int):List[Int] = {
    if(element > maksElement) Nil
    else if(element % 2 == 1) element :: getListOfOddNumbers(element + 1,maksElement)
    else getListOfOddNumbers(element + 1,maksElement)
  }

  // Zadanie 5 5pkt.
  def ldzialanie[A](lpierwsza:LazyList[A],ldruga:LazyList[A],operacja:(A,A)=>A):LazyList[A]={
    (lpierwsza,ldruga) match {
      case (LazyList(),LazyList()) => LazyList()
      case (list1,LazyList()) => list1
      case (LazyList(),list2) => list2
      case (list1,list2)  => operacja(list1.head,list2.head)#::ldzialanie(list1.tail,list2.tail,operacja)
    }
  }

  def sumLists(firstList:List[Int],secondList:List[Int]):List[Int]={
    (firstList,secondList) match{
      case (Nil,Nil) => Nil
      case (f1,Nil) => f1
      case (Nil,f2) => f2
      case (h1::t1,h2::t2) => (h1 + h2) :: sumLists(t1,t2)
    }
  }
}

