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
