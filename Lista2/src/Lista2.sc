//zadanie 1
def product(list: List[Double]): Double =
  if(list.isEmpty) 0
  else if(list.tail!=Nil) list.head * product(list.tail)
  else list.head

product(Nil)==0
product(List(1.7, 2, 5.75, 13.75))==268.8125
product(List(1.27))==1.27
product(List(-8.5, 1.2, 5, 3.4))==(-173.39999999999998) //-173,4: zakres bledu maszyny przy liczeniu

//zadanie 2
def stringBuilder(list: List[String], classifier: String, end: String): String =
  if(list==Nil) end
  else if(list.tail!=Nil) list.head+classifier+stringBuilder(list.tail, classifier, end)
  else list.head+end

stringBuilder(List("My","name","is","Klaudia")," ^ ",".")=="My ^ name ^ is ^ Klaudia."
stringBuilder(List("123","456", "789"), "-", "!")=="123-456-789!"
stringBuilder(List(), "&", ".")=="."
stringBuilder(List(" "," ", " "), "&", ".")==" & & ."

