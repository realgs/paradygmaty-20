//Lista 3 Maciej Kopiński

//ZADANIE 1

def divide(xs: List[Int]):(List[Int], List[Int])={
  xs match{
    case (Nil)=> (Nil, Nil);
    case (h::t)=> if(h<0){
      if(h%2!=0) (h::divide(t)._1,h::divide(t)._2)
      else (h::divide(t)._1,divide(t)._2)
    }else divide(t)
  }
}

divide(List())==(List(), List());
divide(List(-3, -6, 8, -9, 13))==(List(-3, -6, -9), List(-3, -9));

//ZADANIE 2

def listLength[A](xs: List[A]): Int=
  if(xs==Nil) 0
  else 1+listLength(xs.tail)

listLength(List(1, 2, 3, 4))==4;
listLength(List())==0;

//ZADANIE 3

def mergeTwoLists[A](xs: List[A], ys: List[A]): List[A]={
  (xs, ys) match{
    case (Nil, _)=> ys
    case (_, Nil)=> xs
    case(h1::t1, h2::t2)=> h1::h2::mergeTwoLists(t1, t2)
  }
}

mergeTwoLists(List(), List())==List();
mergeTwoLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6))==List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6);
mergeTwoLists(List(1, 2, 3), List())==List(1, 2, 3);

//ZADANIE 4



//ZADANIE 5

def joinLists[A](list1: List[A], list2: List[A], list3: List[A]):List[A]={
  (list1, list2, list3) match{
    case (Nil, Nil, Nil)=> Nil
    case (h::t, _, _)=> h::joinLists(t, list2, list3)
    case (Nil, h::t, _)=> h::joinLists(Nil, t, list3)
    case (Nil, Nil, h::t)=> h::joinLists(Nil, Nil, t)
  }
}

joinLists(List(), List(), List())==List();
joinLists(List(1, 2, 3), List(4, 5), List())==List(1, 2, 3, 4, 5);
joinLists(List(), List(1, 2, 4), List(5, 6, 7))==List(1, 2, 4, 5, 6, 7);
joinLists(List(5, 4, 3, 2), List(1, 0), List(9))==List(5, 4, 3, 2, 1, 0, 9);
joinLists(List(), List(), List())==List();

