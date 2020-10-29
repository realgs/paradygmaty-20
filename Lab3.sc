// Kacper Kochański

// Funkcje pomocnicze
def rev[A](lista:List[A]):List[A]={
  def revHelper[A](lista:List[A],revLista:List[A]):List[A]={
    if(lista == Nil) revLista
    else revHelper(lista.tail,lista.head::revLista)
  }
  revHelper(lista,Nil)
}

def mergeList[A](list1:List[A],list2:List[A]):List[A]={
  def mergeListHelper(list1:List[A],list2:List[A],result:List[A]):List[A]={
    (list1,list2) match{
      case (Nil,Nil) => rev(result)
      case (Nil,head::tail) => mergeListHelper(list1,tail,head::result)
      case (head::tail,_) => mergeListHelper(tail,list2,head::result)
    }
  }
  mergeListHelper(list1,list2,Nil)
}

def stringEquals(f:String,s:String):Boolean={
  def stringEqualsHelper(f:String,s:String):Boolean= {
    if (f == "" && s == "") true
    else if (f == "" && s != "") false
    else if (f != "" && s == "") true
    else f.head == s.head && stringEqualsHelper(f.tail, s.tail)
  }

  def equals(f:String,s:String):Boolean={
    if(f == "") false
    else stringEqualsHelper(f,s) || equals(f.tail,s)
  }

  equals(f,s)
}

// Zadanie 1 (złożoność czasowa - liniowa, złożoność pamięciowa - stała)

def rozdzielListy(lista:List[Double]):(List[Double],List[Double])={
  def rozdzielListyHelper(lista:List[Double],f:List[Double],s:List[Double]):(List[Double],List[Double])= {
    lista match {
      case Nil => (rev(f), rev(s))
      case head :: tail if head < 0 =>
        if (head % 2 == 0) rozdzielListyHelper(tail,head::f,s)
        else rozdzielListyHelper(tail,head::f,head::s)
      case _ => rozdzielListyHelper(lista.tail,f,s)
    }
  }
  rozdzielListyHelper(lista,Nil,Nil)
}

rozdzielListy(Nil) == (Nil,Nil)
rozdzielListy(List(-1,-2,-3,-4)) == (List(-1,-2,-3,-4),List(-1,-3))
rozdzielListy(List(1,-2,3,-4)) == (List(-2,-4),Nil)
rozdzielListy(List(-1,2,-3,4)) == (List(-1,-3),List(-1,-3))
rozdzielListy(List(1.5,2.5,3.6,6.7)) == (Nil,Nil)

// Zadanie 2 (złożoność czasowa - liniowa, złożoność pamięciowa - stała)

def dlugosc[A](lista:List[A]):Int ={
  def dlugoscHelper[A](lista:List[A],rozmiar:Int):Int ={
    if (lista == Nil) rozmiar
    else dlugoscHelper(lista.tail,rozmiar + 1)
  }
  dlugoscHelper(lista,0)
}

dlugosc(Nil) == 0
dlugosc(List(1)) == 1
dlugosc(List("a","b","c","d","e")) == 5
dlugosc(List(1.0,-2.0,3.0)) == 3

// Zadanie 3 (złożonośc obliczeniowa -> liniowa, pamięciowa -> stała)

def polacz[A](fir:List[A],sec:List[A]):List[A]={
  def polaczHelper[A](lista:List[A],fir:List[A],sec:List[A]):List[A]={
    (fir,sec) match{
      case (_,Nil) => mergeList(rev(lista),fir)
      case (Nil,_) => mergeList(rev(lista),sec)
      case (h1::t1,h2::t2) => polaczHelper(h1::h2::lista,t1,t2)

    }
  }
  polaczHelper(Nil,sec,fir)
}

polacz(Nil,Nil) == Nil
polacz(Nil,List(1,2,3,4,5)) == List(1,2,3,4,5)
polacz(List(1,2,3,4,5),Nil) == List(1,2,3,4,5)
polacz(List("a","c","e"),List("b","d")) == List("a","b","c","d","e")
polacz(List("a","c"),List("b","d","e")) == List("a","b","c","d","e")

// Zadanie 4

def find(list:List[String],elem:List[String]):List[String]={
  def compare(listElem:String,elem:List[String]):Boolean={
    if(elem == Nil) false
    else stringEquals(listElem,elem.head) || compare(listElem,elem.tail)
  }
  def findHelper(list:List[String],elem:List[String],result:List[String]):List[String]={
    if (list == Nil || elem == Nil) rev(result)
    else if(compare(list.head,elem)) findHelper(list.tail,elem,list.head::result)
    else findHelper(list.tail,elem,result)
  }
  findHelper(list,elem,Nil)
}

def find2(list:List[String],elem:List[String]):List[String]= {
  def compare(listElem:String,elem:List[String]):Boolean={
    if(elem == Nil) false
    else stringEquals(listElem,elem.head) || compare(listElem,elem.tail)
  }

  def findHelper(list:List[String],elem:List[String]):List[String]={
    if (list == Nil || elem == Nil) Nil
    else if(compare(list.head,elem)) list.head::findHelper(list.tail,elem)
    else findHelper(list.tail,elem)
  }
  findHelper(list,elem)
}

find(Nil,Nil) == Nil
find2(Nil,Nil) == Nil

find(Nil,List("a")) == Nil
find2(Nil,List("a")) == Nil

find(List("a"),Nil) == Nil
find2(List("a"),Nil) == Nil

find(List("a","b","c","d"),List("e")) == Nil
find2(List("a","b","c","d"),List("e")) == Nil

find(List("aaa","bbb","aba","ccc","xer"),List("aa","ab","bb")) == List("aaa","bbb","aba")
find2(List("aaa","bbb","aba","ccc","xer"),List("aa","ab","bb")) == List("aaa","bbb","aba")

find(List("xxxaa","xxaaxx","xaaaxx","ssass"),List("aa")) == List("xxxaa","xxaaxx","xaaaxx")
find2(List("xxxaa","xxaaxx","xaaaxx","ssass"),List("aa")) == List("xxxaa","xxaaxx","xaaaxx")

