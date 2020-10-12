//zadanie 1

def listProduct(list : List[Double]) : Double =
  if(list == Nil) 0
  else if(list.tail != Nil) list.head * listProduct(list.tail)
  else list.head

listProduct(List(80)) == 80
listProduct(List()) == 0
listProduct(List(2.25,2,4,10)) == 180
listProduct(List(1.1,2.2,3.3,99.9))
math.abs(797.8014 - listProduct(List(1.1,2.2,3.3,99.9))) <= 1.0e-12 //true, w zakresie błędu obliczeń
listProduct(List(0,0,0,0,0)) == 0

//zadanie 2

def makeSentence(wordsList : List[String], separator : String, ending : String) : String =
  if(wordsList == Nil) ending
  else if(wordsList.tail != Nil)  wordsList.head + separator + makeSentence(wordsList.tail, separator, ending)
  else wordsList.head + makeSentence(wordsList.tail, separator, ending)

makeSentence(List("Ala","ma","kota")," ** ",".") == "Ala ** ma ** kota."
makeSentence(List()," -- ",".") == "."
makeSentence(List("")," ",".") == "."
makeSentence(List("1","2","3"),"+","?") == "1+2+3?"
makeSentence(List("D","N","A"),"","!") == "DNA!"
makeSentence(List("D","N","A"),"","") == "DNA"

//zadanie 3

def doNumbersFit(numbList : List[Double], X : Double, Y : Double) : Boolean =
  if(numbList == Nil) true
  else (numbList.head >= X && numbList.head <= Y) && doNumbersFit(numbList.tail, X, Y)

doNumbersFit(List(2,3,4,5,6), 2, 5) == false
doNumbersFit(List(1,10,100), 0, 101) == true
doNumbersFit(List(1,10,100), 0.99, 100.001) == true
doNumbersFit(List(1.55,2.6,9.99), 1, 10) == true
doNumbersFit(List(), 1, 10) == true
doNumbersFit(List(0), 0, 0) == true

//zadanie 4

def pow(base: Double, exp: Int) : Double =
  if(exp == 0) 1
  else base * pow(base, exp - 1)

pow(2,6) == 64
pow(2,0) == 1
pow(3.5,4) == 150.0625
pow(0, 10) == 0
pow(0.25, 2) == 0.0625