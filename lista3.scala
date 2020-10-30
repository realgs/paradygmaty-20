
import scala.annotation.tailrec

object lista3{

//funkcje pomocnicze

// O(n)
def reverse[A](lista:List[A]):List[A]= {
  @tailrec
  def reverseInner(lista:List[A],wyj:List[A]):List[A]= {
    if (lista.isEmpty) wyj
    else reverseInner(lista.tail,lista.head+:wyj)
  }
  reverseInner(lista,Nil)
}

// O(n)
@tailrec
def prependReversed[A](lista1:List[A],lista2:List[A]):List[A]=
    if(lista1.isEmpty) lista2
    else prependReversed(lista1.tail,lista1.head+:lista2)

// O(2n)
def appendToEnd[A](lista1:List[A],lista2:List[A]):List[A]={
  @tailrec
  def appendInner(lista1:List[A],lista2:List[A]):List[A]=
    if(lista1.isEmpty)lista2
    else appendInner(lista1.tail,lista1.head+:lista2)
  appendInner(reverse(lista1),lista2)
}

// O(n)
def dlugoscStringa[A](str:String):Int= {
  @tailrec
  def dlSacc(str:String,acc:Int):Int=
    str match {
      case "" => acc
      case _ => dlSacc(str.tail,acc+1)
    }
  dlSacc(str,0)
}

// zad 1
// obliczeniowo O(n), pamieciowo O(2n)
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

// zad 2
// obliczeniowo O(n), pamieciowo O(n+1)
def dlugosc[A](lista:List[A]):Int= {
  @tailrec
  def dlugoscWewn(lista:List[A],acc:Int):Int= {
    if(lista.isEmpty) acc
    else dlugoscWewn(lista.tail,acc+1)
  }
  dlugoscWewn(lista,0)
}

// zad 3
// obliczeniowo O(n+m+|n-m|), pamieciowo O(n+m)
def polacz[A](lista1:List[A],lista2:List[A]):List[A]= {
  @tailrec
  def polaczWewn(lista1: List[A], lista2: List[A], wynik: List[A]): List[A] =
    if (lista1.isEmpty) prependReversed(wynik,lista2)
    else if (lista2.isEmpty) prependReversed(wynik,lista1)
    else polaczWewn(lista1.tail,lista2.tail,lista2.head+:(lista1.head+:wynik))
  polaczWewn(lista1,lista2,List())
}

// zad 4
// rekursja ogonowa (?)
// obliczeniowa O(), pamieciowa O()
@tailrec
def czySekwencjeRowne[A](slowo:String,klucz:String,dlugoscKlucza:Int):Boolean=
  if (dlugoscKlucza==0) true
  else if (slowo.head==klucz.head) czySekwencjeRowne (slowo.tail, klucz.tail, dlugoscKlucza-1)
  else false

@tailrec
def szukajWzorca[A](slowo:String,klucz:String,dlugoscSlowa:Int,dlugoscKlucza:Int):Boolean=
  if (dlugoscSlowa>=dlugoscKlucza)
    if (slowo.head==klucz.head)
      czySekwencjeRowne(slowo,klucz,dlugoscKlucza) || szukajWzorca(slowo.tail,klucz,dlugoscSlowa-1,dlugoscKlucza)
    else szukajWzorca(slowo.tail,klucz,dlugoscSlowa-1,dlugoscKlucza)
  else false

def find[A](slowa:List[String],klucz:String):List[String]= {
  @tailrec
  def findBody[A](slowa:List[String],klucz:String,dlugoscKlucza:Int,acc:List[String]):List[String]=
    slowa match {
      case Nil => acc
      case _ => if (szukajWzorca(slowa.head, klucz, dlugoscStringa(slowa.head), dlugoscKlucza)) findBody(slowa.tail, klucz, dlugoscKlucza, slowa.head +: acc)
      else findBody(slowa.tail, klucz, dlugoscKlucza, acc)
    }
  if (!klucz.isEmpty) findBody(reverse(slowa),klucz,dlugoscStringa(klucz),Nil)
  else slowa
}

// zad 5
// Rekursja ogonowa
// obliczeniowo O(n+m), pamieciowo O(n+m+l)
def joinLists[A](lista1:List[A],lista2:List[A],lista3:List[A]):List[A]=
  appendToEnd(lista1,appendToEnd(lista2,lista3))

def main(args: Array[String]): Unit = println("")

println("Zad 1")
println(podziel(List(-3,-6,8,-9,13)))
println(podziel(List(0,1,1,2,3,4,5,6,7,7,8,9)))
println(podziel(List(-8,-7,-6,-5,-4,-3,-3,-2,-2,-1,0,1,2,3,4,5,6)))

println("\nZad 2")
println(dlugosc(List(-3,-6,8,-9,13)))
println(dlugosc(List("ala",4,"kota")))
println(dlugosc(Nil))

println("\nZad 3")
println(polacz( List(5,4,3,2),List(1,2,3,4,5,6)))
println(polacz( List(5,4,3,2),List(1,2,3,4)))
println(polacz( List(5,4,3,2),List(1,2)))
println(polacz( List(5,4,3,2),List()))
println(polacz( Nil,List(1,2,3,4)))

println("\nZad 4")
println(find(List("Pizza alabama"),"ala"))
println(find(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"),"index0168"))
println(find(List("Ala","ala","Pizza alabama","alla","","kek","aluminium"),"ala"))
println(find(List("zegar","Maciek","rowerowy zderzak"),""))
println(find(List(""),""))
println(find(List(),""))
println(find(List(),"..."))

println("\nZad 5")
println(joinLists(List(5,4,3,2),List(1,0),List(9,10)))
println(joinLists(List(5,4,3,2),List(),List(9,10)))
println(joinLists(List(),List(1,0),List(9,10)))
println(joinLists(List(5,4,3,2),List(1,0),Nil))
}

