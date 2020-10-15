//Jakub Kochański
//Zadanie 1
val listProduct: List[Double] => Double = numbers =>
  if(numbers.isEmpty) throw new Exception("Empty list!!!")
  else if(numbers.length == 1) numbers.head
  else numbers.head * listProduct(numbers.tail)

listProduct(List(1.5))
listProduct(List(0,1,2,0))
listProduct(List(-1.4,-2.5,3.1,0.4))

//Zadanie 2
val constructASentence: (List[String],Char,Char) => String = (words,endingSign,separator) =>
  if(words.isEmpty) ""
  else if(words.length == 1) s"${words.head}$endingSign"
  else s"${words.head}$separator${constructASentence(words.tail,endingSign,separator)}"

constructASentence(List("This","is","a","test"),'.',' ')
constructASentence(List(),'.',' ')
constructASentence(List("Test"),'!',',')
constructASentence(List("Some input"),'?','s')