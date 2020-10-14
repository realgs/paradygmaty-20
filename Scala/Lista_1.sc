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
def checkIfInRange(x: List[Double], beginning: Double, end: Double):Boolean={
  //if(x==Nil) throw new Exception("Pusta lista!")
  if(x==Nil) false
  else{
    if(x.size==1 && x.head>=beginning && x.head<=end) true
    else if(x.head>=beginning && x.head<=end) checkIfInRange(x.tail, beginning, end)
    else false
  }
}

checkIfInRange(List(1.2, 2, 4, 5.5), 1, 7);
checkIfInRange(List(1.2, 2, 4, 5.5), 1.3, 7);
checkIfInRange(List(), 1, 7);
checkIfInRange(Nil, 1, 7);


//ZADANIE 4
def power(x:Double,p:Double):Double={
  if(p==0) 1
  else if(p<0) (1/x)*power(x, p+1)
  else x+power(x, p-1)
}

println(power(2.4, 3));
println(power(2.4, -3));
println(power(2.4, 0));