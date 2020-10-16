// Zadanie 1
val multiplyList: List[Double] => Double = list =>{
  if (list == Nil) 0
  else if(list.tail == Nil) list.head
  else list.head * multiplyList(list.tail)
}


println(multiplyList(List()) == 0)
println(multiplyList(List(10)) == 10)
println(multiplyList(List(1,2,-3,-4,5,6,7)) == 5040)
println(multiplyList(List(1.0,2,3.4,4,5.2,6,-7.8)) == -6619.392)


// Zadanie 2
val mergeStrings: (List[String],String,String) => String = (list,sep,end) =>{
  if(list == Nil) ""
  else if (list.tail == Nil) list.head + end
  else list.head + sep + mergeStrings(list.tail,sep,end)
}

println(mergeStrings(Nil,",","!") == "")
println(mergeStrings(List("raz","dwa","trzy"),",","!") == "raz,dwa,trzy!")
println(mergeStrings(List("jedno slowo"),",",".") == "jedno slowo.")


// Zadanie 3
val valInRange: (Double,Double,List[Double]) => Boolean = (X,Y,list) =>{
  if (X > Y) throw new Exception("X nie moze byc wieksze od Y")
  else if(list == Nil) true
  else (list.head >= X && list.head <= Y) && valInRange(X,Y,list.tail)
}

println(valInRange(0,1,Nil) == true)
println(valInRange(0,0,List(0,0,0,0,0,0,0,0,0,0)) == true)
println(valInRange(1,9,List(1,2,3,4,5,6,7,8,9,10)) == false)
println(valInRange(-10.5,10.5,List(1.2,3.4,5.6,7.8,9.10)) == true)
println(valInRange(1,0.5,List(-0.5,0,0.5,1)))
