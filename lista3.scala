import scala.annotation.tailrec

def reverse[A](lista:List[A]):List[A]= {
  @tailrec
  def reverseInner(lista:List[A],wyj:List[A]):List[A]= {
    if (lista.isEmpty) wyj
    else reverseInner(lista.tail,lista.head+:wyj)
  }
  reverseInner(lista,Nil)
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
