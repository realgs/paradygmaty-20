// Zadanie 1
def productOfList (xs: List[Double]): Double=
  if(xs.nonEmpty) {
      if (xs.length > 1) xs.head * productOfList(xs.tail)
      else xs.head
  } else 0.0

productOfList(List(2.0,3.0,4.0))==24.0
productOfList(List(0.0,3.0,4.0))==0.0
productOfList(List())==0.0
productOfList(List(2.0,3.0,4.0,-0.5))== -12.0
productOfList(List(2.0,-3.0,-4.0))==24.0

// Zadanie 2
def makeSentence(lOfStrings: List[String],endSymbol:String,separator:String):String={
  if(lOfStrings.nonEmpty){
      if(lOfStrings.length > 1) lOfStrings.head+separator+makeSentence(lOfStrings.tail,endSymbol,separator)
      else lOfStrings.head+endSymbol
  }
  else endSymbol
}
makeSentence(List("Lista","Druga"),"."," ").equals("Lista Druga.")
makeSentence(List("Ala","ma","kota"),"?"," , ").equals("Ala , ma , kota?")
makeSentence(List(),"."," ").equals(".")
makeSentence(List("a","l","a"),",","").equals("ala,")
makeSentence(List("One"),".","   ").equals("One.")

// Zadanie 3
def checkNumbers (listOfNumbers: List[Double],X:Double,Y:Double): Boolean =
    if(listOfNumbers.nonEmpty){
          if(listOfNumbers.head>=X && listOfNumbers.head<=Y)
              if (listOfNumbers.length>1) checkNumbers(listOfNumbers.tail, X, Y)
              else true
          else false
    }
    else false

!checkNumbers(List(), 0.5, 4.0)
checkNumbers(List(0.5, 2.0, 4.0), 0.5, 4.0)
checkNumbers(List(1.0,1.0,1.0),1.0,1.0)
!checkNumbers(List(4.5,2.0,3.0),0.5,4.0)
!checkNumbers(List(-1.0,-2.0,-3.0),0.5,4.0)

// zadanie 4
def powerOfX(x:Double,exponent:Int):Double=
    if(exponent>0)x*powerOfX(x,exponent-1)
    else if(exponent<0) 1/x*powerOfX(x,exponent+1)
    else 1.0


powerOfX(1.0,2)==1
powerOfX(0.0,0)==1
powerOfX(-3.0,3)== -27
powerOfX(2.0,-1)==0.5
powerOfX(-2.0,-3)== -0.125
