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
// Zlozonosc czasowa oraz pamięciowa to O(n) dla ujemnych oraz w najgorszym wypadku również O(n) dla nieparzystych, zatem O(n^2) dla funkcji.
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

  val negative = filter(arr, checkNegative)
  (negative, filter(negative, checkOdd))
}

divide(List(-1, 0, 5, 2, -5, -2, -20, -15, 0, -5, 2))
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
  def lengthTail(arr: List[Any], n: Int):Int =
    arr match {
      case Nil => n
      case _ => lengthTail(arr.tail, n + 1)
    }
  lengthTail(arr, 0)
}

length(List(5,4,3,2))
length(List("a","b","c","d","e"))
length(List())

// 3)
// Złożoność obliczeniowa O(n) = O(2(arr1.length + arr2.length)), a pamięciowa O(1)
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
// Zlozonosc czasowa to srednio O(el.length + phrase.length), ale w najgorszym przypadku to O(el.length * phrase.length)
@tailrec
def stringContains(el: String, phrase: String):Boolean = {
  @tailrec
  def stringContainsInner(el: String, phrase: String):Boolean =
    (el, phrase) match {
      case(_, "") => true
      case("", _) => false
      case(_, _) => if(el.head == phrase.head) stringContainsInner(el.tail, phrase.tail) else false
    }

    if(el == "")
      false
    else if(el.head == phrase.head && stringContainsInner(el.tail, phrase.tail))
      true
    else
      stringContains(el.tail, phrase)
}
// Zlozonosc czasowa to O(n) + zlozonosc dla "stringContains", zatem srednio O(arr.length(el.length + phrase.length))
def find(arr: List[String], phrase: String): List[String] =
  arr match {
    case Nil => Nil
    case hd::tl => if(stringContains(hd, phrase)) hd::find(tl, phrase) else find(tl, phrase)
  }

// Zlozonosc czasowa to O(arr.length + res.length), z powodu reverse + zlozonosc dla "stringContains", zatem srednio O(arr.length(el.length + phrase.length) + res.length)
def findTail(arr: List[String], phrase: String): List[String] = {
  @tailrec
  def findTailInner(arr: List[String], phrase: String, res:List[String]): List[String] =
    arr match {
      case Nil => reverse(res)
      case hd::tl => if(stringContains(hd, phrase)) findTailInner(tl, phrase, hd::res) else findTailInner(tl, phrase, res)
    }

  findTailInner(arr, phrase, Nil)
}

// Zlozonosc czasowa "pętli" to O(arr.length * phrases.length), musimy wziąć pod uwagę wykonywanie stringContains, zatem zlożoność funkcji to średnio O(arr.length * phrases.length * (el.length + phrase.length))
def findList(arr: List[String], phrases: List[String]): List[String] = {
  def containPhrases(text: String, phrases: List[String]):Boolean =
    phrases match {
    case Nil => false
    case hd::tl => if(stringContains(text, hd)) true else containPhrases(text, tl)
  }

  arr match {
    case Nil => Nil
    case hd::tl => if(containPhrases(hd, phrases)) hd::findList(tl, phrases) else findList(tl, phrases)
  }
}

find(List("apple", "orange", "banana", "peach", "strawberry", "blackberry"), "an")
findTail(List("apple", "orange", "banana", "peach", "strawberry", "blackberry"), "an")

findList(List("apple", "orange", "banana", "peach", "strawberry", "blackberry"), List("berry", "nana"))

// 5)
// Złożoność obliczeniowa i pamięciowa O(n), gdzie n to suma dlugosci list
def join[A](arr1: List[A], arr2: List[A], arr3: List[A]): List[A] =
  (arr1, arr2, arr3) match {
    case (head::tail,_,_) => head :: join(tail, arr2, arr3)
    case (Nil,head::tail,_) => head :: join(Nil, tail, arr3)
    case (Nil,Nil,_) => arr3
  }

// Złożoność obliczeniowa O(2n), a pamięciowa O(1), gdzie n to suma dlugosci list
def joinTail[A](arr1: List[A], arr2: List[A], arr3: List[A]): List[A] = {
  def joinTailInner(arr1: List[A], arr2: List[A], arr3: List[A], res: List[A]): List[A]=
    (arr1, arr2, arr3) match {
      case (head::tail,_,_) => joinTailInner(tail, arr2, arr3, head :: res)
      case (Nil,head::tail,_) => joinTailInner(Nil, tail, arr3, head :: res)
      case (Nil,Nil,_) => reverse(arr3 ::: res)
    }

  joinTailInner(arr1, arr2, arr3, Nil)
}

join(List(1,2,3), List(4,5), List(6,7,8))
joinTail(List(1,2,3), List(4,5), List(6,7,8))


