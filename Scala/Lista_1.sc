//ZADANIE 1
def multiply(x:List[Double]):List[Double]={
  if(x==Nil) Nil
  else x.head*x.head::multiply(x.tail)
}

println(multiply(List(1.2, 3, 4, 5)));

//ZADANIE 2
def concatenate(x: List[String], sep: Char, end:Char):String={
  if(x==Nil) end.toString
  else if(x.size==1) x.head+end
  else x.head+sep+concatenate(x.tail, sep, end)
}

println(concatenate(List("Ala", "ma", "kota"), ',', '.'));

//ZADANIE 3
def checkIfInRange(x: List[Double]):Boolean={
true
}

//ZADANIE 4
def power(x:Double,y:Double):Double={
1
}