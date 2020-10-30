import scala.annotation.tailrec

class L3 {
// własna implementacja metody reverse
  def reverse[A](xs1:List[A], xs:List[A]):List[A] ={
    if(xs1==Nil) xs
    else reverse(xs1.tail,xs1.head::xs)
  }


  // 1)
  def splitTable(xs:List[Int]):(List[Int],List[Int]) ={
    def splitNegative(xs: List[Int]): List[Int] ={
      if (xs == Nil) Nil
      else if (xs.head < 0) xs.head :: splitNegative(xs.tail)
      else splitNegative(xs.tail)
    }

    def splitOddNegative(xs: List[Int]): List[Int] ={
      if (xs == Nil) Nil
      else if (xs.head < 0 && xs.head % 2 != 0) xs.head :: splitOddNegative(xs.tail)
      else splitOddNegative(xs.tail)
    }

    (splitNegative(xs), splitOddNegative(xs))
  }


  // 2)
  // Złożoność obliczeniowa: O(n), n - długość listy wejściowej
  // Złożoność pamięciowa: O(1)
  def length[A](xs:List[A]):Int ={
    @tailrec
    def lengthHelper(length:Int, xs:List[A]):Int ={
      if(xs==Nil) length
      else lengthHelper(length+1,xs.tail)
    }
    lengthHelper(0,xs)
  }


  // 3)
  // Złożoność obliczeniowa: O(4 * n), n - długość krótszej listy wejściowej
  // Złożoność pamięciowa: O(1)
  def mergeLists[A](xs1:List[A],xs2:List[A]):List[A] ={
    @tailrec
    def mergeListsHelper(xs1:List[A], xs2:List[A], xs:List[A]):List[A] = {
      (xs1, xs2) match {
        case (Nil, _) => reverse(xs,xs2)
        case (_, Nil) => reverse(xs,xs1)
        case (_, _) => mergeListsHelper(xs1.tail, xs2.tail,xs2.head :: xs1.head :: xs)
      }
    }
    mergeListsHelper(xs1,xs2,Nil)
  }


  // 4)
  // Złożoność obliczeniowa: O(n), n - długość stringa
  // Złożoność pamięciowa: O(1)
  def strContains(string:String,pattern:String):Boolean ={
    @tailrec
    def strContainsHelper(stringB:String, patternB:String):Boolean ={
      if(patternB.isEmpty) true
      else if(stringB.isEmpty) false
      else{
        if(stringB.head==patternB.head) strContainsHelper(stringB.tail,patternB.tail)
        else {
          if(stringB.head==pattern.head) strContainsHelper(stringB.tail, pattern.tail)
          else strContainsHelper(stringB.tail, pattern)
        }
      }
    }
    if(pattern.isEmpty) true
    else if(string.isEmpty) false
    else strContainsHelper(string,pattern)
  }

  //rekursja nieogonowa
  // Złożoność obliczeniowa: O(n * k), n - średnia długość stringów; k - długość listy wejściowej
  // Złożoność pamięciowa: O(k), k - długość listy wejściowej
  def findRec(list:List[String],pattern:String):List[String] ={
    if(list==Nil) Nil
    else if(strContains(list.head,pattern)) list.head::findRec(list.tail,pattern)
    else findRec(list.tail,pattern)
  }

  //rekursja ogonowa
  // Złożoność obliczeniowa: O(n * k + m), n - średnia długość stringów; k - długość listy wejściowej;
  // m - długość listy wynikowej
  // Złożoność pamięciowa: O(1)
  def findTailRec(list:List[String],pattern:String):List[String] ={
    @tailrec
    def findTailRecHelper(list:List[String], newList:List[String]):List[String] ={
      if(list==Nil) reverse(newList,Nil)
      else if(strContains(list.head,pattern)) findTailRecHelper(list.tail,list.head::newList)
      else findTailRecHelper(list.tail,newList)
    }
    findTailRecHelper(list,Nil)
  }

  //N fraz wejsciowych
  // Złożoność obliczeniowa: O(n * k), n - długość stringa; k - ilość wzorców
  // Złożoność pamięciowa: O(1)
  def listStrContains(string:String, patterns: List[String]):Boolean = {
    if (patterns == Nil) false
    else if (strContains(string, patterns.head)) true
    else listStrContains(string, patterns.tail)
  }

  //rekursja nieogonowa
  // Złożoność obliczeniowa: O(n * m * k), n - średnia długość stringów; m - długość listy wejściowej;
  // k - ilość wzorców
  // Złożoność pamięciowa: O(m), m - długość listy wejściowej
  def findMultiRec(list:List[String],patterns:List[String]):List[String] ={
    if(list==Nil||patterns==Nil) Nil
    else if(listStrContains(list.head,patterns)) list.head::findMultiRec(list.tail,patterns)
    else findMultiRec(list.tail,patterns)
  }

  //rekursja ogonowa
  // Złożoność obliczeniowa: O(n * m * k + l), n - średnia długość stringów; m - długosć listy wejściowej;
  // k - ilość wzorców; l - długość listy wynikowej
  // Złożoność pamięciowa: O(1)
  def findMultiTailRec(list:List[String],patterns:List[String]):List[String] ={
    @tailrec
    def findMultiTailRecHelper(list:List[String], newList:List[String]):List[String] ={
      if(list==Nil||patterns==Nil) reverse(newList,Nil)
      else if(listStrContains(list.head,patterns)) findMultiTailRecHelper(list.tail,list.head::newList)
      else findMultiTailRecHelper(list.tail,newList)
    }
    findMultiTailRecHelper(list,Nil)
  }


  // 5)
  //rekursja nieogonowa
  // Złożoność obliczeniowa: O(n + m), n - długość listy1; m - długosć listy2
  // Złożoność pamięciowa: O(n + m), n - długość listy1; m - długosć listy2
  def joinListsRec[A](xs1:List[A],xs2:List[A],xs3:List[A]):List[A] ={
    (xs1,xs2,xs3)match {
    case (head::tail,_,_) => head :: joinListsRec (tail, xs2, xs3)
    case (Nil,head::tail,_) => head :: joinListsRec (Nil, tail, xs3)
    case (Nil,Nil,_) => xs3
    }
  }

  //rekursja ogonowa
  // Złożoność obliczeniowa: O(n + m + k + l), n - długość listy1; m - długosć listy2;
  // k - długość listy3; l - długość listy wynikowej
  // Złożoność pamięciowa: O(1)
  def joinListsTailRec[A](xs1:List[A],xs2:List[A],xs3:List[A]):List[A] ={
    @tailrec
    def joinListHelper(xs1:List[A], xs2:List[A], xs3:List[A], xs:List[A]):List[A] ={
      (xs1,xs2,xs3)match {
        case (head::tail,_,_) => joinListHelper(tail,xs2,xs3,head::xs)
        case (Nil,head::tail,_) => joinListHelper(Nil,tail,xs3,head::xs)
        case (Nil,Nil,head::tail) => joinListHelper(Nil,Nil,tail,head::xs)
        case (Nil,Nil,Nil) => reverse(xs,Nil)
      }
    }
    joinListHelper(xs1,xs2,xs3,Nil)
  }
}
