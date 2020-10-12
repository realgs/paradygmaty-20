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
