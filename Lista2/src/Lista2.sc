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


//zadanie 3
def intervalChecker(list: List[Double], X: Double, Y: Double): Boolean =
  if(list==Nil) false
  else if((list.head>=X) && (list.head<=Y) && (list.tail==Nil))true
  else if((list.head>=X) && (list.head<=Y)) intervalChecker(list.tail, X, Y)
  else false

intervalChecker(List(1.4, 2, 3.5, 2.2), 0, 4)==true
intervalChecker(List(1.6, -2, 4.5, 2.2), 0, 5)==false
intervalChecker(List(1.6, 13, 4.5, 4.2), 0, 10)==false
intervalChecker(List(), 0, 3.3)==false
intervalChecker(List(1.1,-2.2), -2.2, 1.1)==true















