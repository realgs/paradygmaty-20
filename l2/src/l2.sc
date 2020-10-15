import scala.annotation.tailrec

//Zadanie 1
def product(xs : List[Double]):Double =
  if (xs.nonEmpty)
    if (xs.length>1)
      xs.head*product(xs.tail)
    else xs.head
  else 0.0

product(List(1.0,2.0,3.0)) == 6.0
product(List(1.0,1.0,1.0,-1.0,1.0)) == -1.0
product(List(6.0,8.0,1.0,-1.0,1.0)) == -48.0
product(List(5.0,-1.7,3.9)) == -33.15
product(List()) == 0.0

//Zadanie 2
def combineWords (xs : List[String],last_Sign : String, separator:String ): String =
  if (xs.nonEmpty)
    if (xs.length>1)
        xs.head + separator + combineWords(xs.tail, last_Sign, separator)
    else xs.head + last_Sign
  else ""

combineWords(List("Ala","ma","kota"),"."," ") == "Ala ma kota."
combineWords(List("Tu","nie","wolno","palic"),"!"," ") == "Tu nie wolno palic!"
combineWords(List("Co","ma","Ala"),"?"," ") == "Co ma Ala?"
combineWords(List("Kup","jajka","make"),".",", ") == "Kup, jajka, make."
combineWords(List(),"."," ") == ""

//Zadanie 3
@tailrec
def checkNumbers (xs : List[Double], X : Double, Y : Double):Boolean =
   if (xs.nonEmpty)
     if (xs.head>=X && xs.head<=Y) checkNumbers(xs.tail,X,Y)
     else false
   else true

checkNumbers(List(1.0,2.0,3.0),1.0,3.0) == true
checkNumbers(List(-5.8,1.0,2.0,3.0,40.7),-6.0,10.0) == false
checkNumbers(List(),1.0,3.0) == true
checkNumbers(List(4.0,7.0,38.0,-3.6),-4.0,40.5) == true
checkNumbers(List(0.0, 3.5, 0.00001),1.0,3.0) == false

//Zadanie 4
def toPower (base : Double, exponent : Int):Double =
    if (exponent > 0) base*toPower(base,exponent-1)
    else if (exponent < 0) (1/base)*toPower(base,exponent+1)
    else 1.0

toPower(2.0,0) == 1.0
toPower(2.0,3) == 8.0
toPower(2.0,-2) == 0.25
toPower(-2.5,9) == -3814.697265625
toPower(0.0,1000) == 0.0