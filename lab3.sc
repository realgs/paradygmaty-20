import scala.annotation.tailrec
//  Karol Waliszewski

// Helpers
def reverse[A](arr:List[A]):List[A] = {
  @tailrec
  def reverseTail(arr:List[A], res:List[A]):List[A] =
    arr match {
      case Nil => res
      case _ => reverseTail(arr.tail, arr.head :: res)
    }

  reverseTail(arr, Nil)
}


// 1)


def divide(arr: List[Int]):(List[Int], List[Int]) = {
  def filter[A](arr:List[A], conditional: A => Boolean):List[A] =
    arr match {
      case head::tail => if(conditional(head)) head :: filter(tail, conditional) else filter(tail, conditional)
      case Nil => Nil
    }

  def checkNegative(n:Int):Boolean =
    n < 0

  def checkOdd(n:Int):Boolean =
    (n % 2 + 2) % 2 == 1

  def checkNegativeAndOdd(n:Int):Boolean =
    checkNegative(n) && checkOdd(n)

   (filter(arr, checkNegative), filter(arr, checkNegativeAndOdd))
}

divide(List(-1, 0, 5, 2, -5, -2))
divide(List(1, 2, 3, 4, 5))
divide(List(-2, 0, 5, 2, -4, -6))

// 2)

// Złożoność obliczeniowa O(n), a pamięciowa O(n)
//def length(arr: List[Any]):Int =
//  arr match {
//    case Nil => 0
//    case _ => 1 + length(arr.tail)
//  }

// Złożoność obliczeniowa O(n), a pamięciowa O(1)
def length(arr: List[Any]):Int = {
  @tailrec
  def lengthIter(arr: List[Any], n: Int):Int =
    arr match {
      case Nil => n
      case _ => lengthIter(arr.tail, n + 1)
    }
  lengthIter(arr, 0)
}

length(List(5,4,3,2))
length(List("a","b","c","d","e"))
length(List())

// 3)
def connect[A](arr1: List[A], arr2: List[A]): List[A] = {
  @tailrec
  def connectTail(arr1: List[A], arr2: List[A], result: List[A], n: Int ): List[A] =
    (arr1, arr2, n % 2) match {
      case (Nil, Nil, _) => reverse(result)
      case (head::tail, Nil, _) => connectTail(tail, Nil, head :: result, n + 1)
      case (Nil, head::tail, _) => connectTail(Nil, tail, head :: result, n + 1)
      case (_, _, 0) => connectTail(arr1.tail, arr2, arr1.head :: result, n + 1)
      case (_, _, 1) => connectTail(arr1, arr2.tail, arr2.head :: result, n + 1)
    }
  connectTail(arr1, arr2, Nil ,0)
}

connect(List(1,3,5,7),List(2,4,6,8))
connect(List(1,3),List(2,4,5,6,7,8))
connect(List(1,3,5,6,7,8),List(2,4))
connect(List(1,2,3),List())
connect(List(),List(1,2,3))

// 4)
def find[String](arr: List[String], el: String): List[String] =
  Nil

// 5)
def join[A](arr1: List[A], arr2: List[A], arr3: List[A]): List[A] =
  (arr1, arr2, arr3) match {
    case (head::tail,_,_) => head :: join(tail, arr2, arr3)
    case (Nil,head::tail,_) => head :: join(Nil, tail, arr3)
    case (Nil,Nil,head::tail) => head :: join(Nil, Nil, tail)
    case (Nil,Nil,Nil) => Nil
  }

join(List(1,2,3), List(4,5), List(6,7,8))