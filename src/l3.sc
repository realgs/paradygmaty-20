import scala.annotation.tailrec
//Lista 3 Maciej Kopiński

//useful
def reverse[A](list : List[A]) : List[A] = {
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

//ZADANIE 1

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

def find[String](xs: List[String], elem: List[String]):List[String]={
  @tailrec
  def inner(accum: List[String], list: List[String]):List[String]={
    list match {
      case Nil=> accum
      case h::t=> {
        @tailrec
        def check(a: Int, length: Int, el: String, str: String):Boolean={
          if(elem==Nil || a>=length) true
          else if(el!=str.toString().charAt(a)) false
          else check(a+1, length, el, str)
        }
        if(check(0, h.toString().length(), elem.head, h)) inner(h::accum, t)
        else Nil
      }
    }
  }
  inner(Nil, elem)
}

find(List("index0169'", "index0168202", "index0168211",
  "index0168210", "index0169222", "index0169224"),
  List("index0168"))==List("index0168211", "index0168210")

//ZADANIE 5

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

