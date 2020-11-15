import scala.annotation.tailrec
//Lista 3 Maciej Kopiński

//useful
def reverse[A](list : List[A]) : List[A] = {
  @tailrec
  def inner(acc : List[A], list : List[A]) : List[A] = {
    list match {
      case Nil => acc
      case (h :: t) => {
        inner(h :: acc, t)
      }
    }
  }
  inner(Nil, list)
}

def reverseTwo[A](list1: List[A], list2: List[A]):(List[A], List[A])={
  @tailrec
  def inner(accum1: List[A], accum2: List[A], xs: List[A], ys: List[A]):(List[A], List[A])={
    (xs, ys) match{
      case (Nil, Nil)=> (accum1, accum2)
      case (Nil, h::t)=> inner(accum1, h::accum2, xs, t)
      case (h::t, Nil)=> inner(h::accum1, accum2, t, ys)
      case (h1::t1, h2::t2)=> inner(h1::accum1, h2::accum2, t1, t2)
    }
  }
  inner(Nil, Nil, list1, list2)
}

//zlozonosc czasowa i pamieciowa liniowa
def contains(main: String, substring: String): Boolean = {
  (main, substring) match {
    case (_, "")=> true
    case ("", _)=> false
    case (_, _)=> {
      def ifPrefix(str1: String, str2: String):Boolean={
        (str1, str2) match {
          case ("", _)=> false
          case (_, "")=> true
          case (_, _)=> if(str1.head==str2.head) ifPrefix(str1.tail, str2.tail) else false
        }
      }
      if(ifPrefix(main, substring)) true
      else (ifPrefix(main.tail, substring.tail))
    }
  }
}

//zlozonosc kwadratowa
def containsAny(main: String, substrings: List[String]): Boolean = {
  (main, substrings) match {
    case ("", _)=> false
    case (_, Nil)=> false
    case (_, h::t)=> {
      if(contains(main, h)) true
      else containsAny(main, t)
    }
  }
}


//ZADANIE 1

//zlozonosc czasowa liniowa wzgledem listy wejsciowej
//zlozonosc pamieciowa stala
def divide(xs: List[Int]):(List[Int], List[Int])={
  @tailrec
  def inner(accum1: List[Int], accum2: List[Int], list: List[Int]):(List[Int], List[Int])={
    list match{
      case (Nil)=> (accum1, accum2);
      case (h::t)=> if(h<0){
        if(h%2!=0) inner(h::accum1, h::accum2, t)
        else inner(h::accum1,accum2, t)
      }else inner(accum1, accum2, t)
    }
  }
  reverseTwo(inner(Nil, Nil, xs)._1, inner(Nil, Nil, xs)._2)
  //

}

divide(List())==(List(), List());
divide(List(-3, -6, 8, -9, 13))==(List(-3, -6, -9), List(-3, -9));
divide(List(-2, -4))==(List(-2, -4), List());
divide(List(1, 2, 0, 3))==(List(), List());

//ZADANIE 2

//zlozonosc pamieciowa stala
//zlozonosc czasowa liniowa
def listLength[A](xs: List[A]): Int= {
  @tailrec
  def inner(acc: Int, x: List[A]):Int={
    if(x==Nil) acc
    else inner(acc+1, x.tail)
  }
  inner(0, xs);
}

listLength(List(1, 2, 3, 4))==4;
listLength(List())==0;

//ZADANIE 3

//zlozonosc pamieciowa stala
//zlozonosc czasowa liniowa
def mergeTwoLists[A](xs: List[A], ys: List[A]): List[A]={
  @tailrec
  def inner(newList: List[A], list1: List[A], list2: List[A]):List[A]={
    (list1, list2) match{
      case (Nil, Nil)=> newList
      case (Nil, h::t)=> inner(h::newList, Nil, t)
      case (h::t, Nil)=> inner(h::newList, t, Nil)
      case(h1::t1, h2::t2)=> inner(h2::h1::newList, t1, t2)
    }
  }
  reverse(inner(Nil, xs, ys))

}

mergeTwoLists(List(), List())==List();
mergeTwoLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6))==List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6);
mergeTwoLists(List(1, 2, 3), List())==List(1, 2, 3);

//ZADANIE 4

//zlozonosc pamieciowa stala
//zlozonosc czasowa m*n
def find(elements: List[String], searched: String):List[String]={
  @tailrec
  def inner(elems: List[String], accum: List[String]):List[String]={
    elems match {
      case Nil=> reverse(accum)
      case h::t=> {
        if(contains(h, searched)) inner(t, h::accum)
        else inner(t, accum)
      }
    }
  }
  inner(elements, Nil)
}

//zlozonosc pamieciowa kwadratowa
//zlozonosc czasowa m*n^2
def findList(elements: List[String], searched: List[String]):List[String]={
  @tailrec
  def inner(elems: List[String], patterns: List[String], accum: List[String]):List[String]={
    if(patterns==Nil) Nil
    elems match {
      case Nil=> reverse(accum)
      case h::t=> {
        if(containsAny(h, patterns)) inner(t, patterns, h::accum)
        else inner(t, patterns, accum)
      }
    }
  }
  inner(elements, searched, Nil)
}

find(List("index0169'", "index0169202", "index0168211",
  "index0168210", "index0169222", "index0169224"),
  "index0168")==List("index0168211", "index0168210")

findList(List("index0169'", "index0167202", "index0168211",
  "index0168210", "index0169222", "index0169224"),
  List("index0168", "index0169"))

//ZADANIE 5

//zlozonosc pamieciowa stala
//zlozonosc czasowa liniowa
def joinLists[A](list1: List[A], list2: List[A], list3: List[A]):List[A]={
  @tailrec
  def inner(newList: List[A], xs: List[A], ys: List[A], zs: List[A]):List[A] ={
    (xs, ys, zs) match{
      case (Nil, Nil, Nil)=> newList
      case (h::t, _, _)=> inner(h::newList, t, ys, zs)
      case (Nil, h::t, _)=> inner(h::newList, Nil, t, zs)
      case (Nil, Nil, h::t)=> inner(h::newList, Nil, Nil, t)
    }
  }
  reverse(inner(Nil, list1, list2, list3))
}

joinLists(List(), List(), List())==List();
joinLists(List(1, 2, 3), List(4, 5), List())==List(1, 2, 3, 4, 5);
joinLists(List(), List(1, 2, 4), List(5, 6, 7))==List(1, 2, 4, 5, 6, 7);
joinLists(List(5, 4, 3, 2), List(1, 0), List(9))==List(5, 4, 3, 2, 1, 0, 9);

