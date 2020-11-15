//Lista 4 Maciej Kopiński

//drzewo
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

//Zadanie 1

def createTree(n: Int, start: Int, end: Int): BT[Int]={

}

//Zadanie 2

def mergeTrees(first: BT[Int], second: BT[Int]): BT[Int]={

}

//Zadanie 3

def deleteDuplicates(first: BT[Int], sexond: BT[Int]): (BT[Int], BT[Int])={

}

//Zadanie 4

def eachNElement[A](stream: Stream[A], n: Int, m: Int): Stream[A]={

}

//Zadanie 5

def ldzialanie(stream1: Stream[Int], stream2: Stream[Int], function: Int): Stream[Int]={
  
}
