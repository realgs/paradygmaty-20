import scala.annotation.tailrec

def reverse[A](lista:List[A]):List[A]= {
  @tailrec
  def reverseInner(lista:List[A],wyj:List[A]):List[A]= {
    if (lista.isEmpty) wyj
    else reverseInner(lista.tail,lista.head+:wyj)
  }
  reverseInner(lista,Nil)
}

def prependReversed[A](lista1:List[A],lista2:List[A]):List[A]={
  @tailrec
  def prependReversedInner(lista1:List[A],lista2:List[A]):List[A]=
    if(lista1.isEmpty)lista2
    else prependReversedInner(lista1.tail,lista1.head+:lista2)
  prependReversedInner(lista1,lista2)
}

def appendToEnd[A](lista1:List[A],lista2:List[A]):List[A]={
  @tailrec
  def appendInner(lista1:List[A],lista2:List[A]):List[A]=
    if(lista1.isEmpty)lista2
    else appendInner(lista1.tail,lista1.head+:lista2)
  appendInner(reverse(lista1),lista2)
}

def podziel[A](listaWej: List[Int]):(List[Int],List[Int])= {
  @tailrec
  def podzielWewn(listaWej: List[Int],wyj:(List[Int],List[Int])):(List[Int],List[Int])= {
    if (listaWej.isEmpty) wyj
    else if(listaWej.head%2==(-1)) podzielWewn(listaWej.tail,(listaWej.head+:wyj._1,listaWej.head+:wyj._2))
    else if(listaWej.head<0) podzielWewn(listaWej.tail,(listaWej.head+:wyj._1,wyj._2))
    else podzielWewn(listaWej.tail,(wyj._1,wyj._2))
   }
  podzielWewn(reverse(listaWej),(Nil,Nil))
}

podziel(List(-3,-6,8,-9,13))

def dlugosc[A](lista:List[A]):Int= {
  @tailrec
  def dlugoscWewn(lista:List[A],acc:Int):Int= {
    if(lista.isEmpty) acc
    else dlugoscWewn(lista.tail,acc+1)
  }
  dlugoscWewn(lista,0)
}

dlugosc(List(-3,-6,8,-9,13))

def polacz[A](lista1:List[A],lista2:List[A]):List[A]= {
  @tailrec
  def polaczWewn(lista1: List[A], lista2: List[A], wynik: List[A]): List[A] =
    if (lista1.isEmpty) prependReversed(wynik,lista2)
    else if (lista2.isEmpty) prependReversed(wynik,lista1)
    else polaczWewn(lista1.tail,lista2.tail,lista2.head+:(lista1.head+:wynik))
  polaczWewn(lista1,lista2,List())
}

polacz( List(5,4,3,2),List(1,2,3,4,5,6))
polacz( List(5,4,3,2),List(1,2,3,4))
polacz( List(5,4,3,2),List(1,2))
polacz( List(5,4,3,2),List())
polacz( Nil,List(1,2,3,4))

def joinLists[A](lista1:List[A],lista2:List[A],lista3:List[A]):List[A]=
  appendToEnd(lista1,appendToEnd(lista2,lista3))

joinLists(List(5,4,3,2),List(1,0),List(9,10))
joinLists(List(5,4,3,2),List(),List(9,10))
joinLists(List(),List(1,0),List(9,10))
joinLists(List(5,4,3,2),List(1,0),Nil)
