
object l5 {
  
//zadanie pierwszse 2,5p
def duplicate[A](l1: List[A], l2: List[Int]):List[A]={
  def duplicateHelper[A](lista1: List[A], lista2: List[Int], lista3: List[A]): List[A]={
    if(lista1.isEmpty || lista2.isEmpty)   return lista3
    (lista1,lista2) match {
      case (hd1::tl,hd2::tl2) =>
        return duplicateHelper(tl,tl2,lista3:::addElemetsNTimes(hd1,hd2,List()))
    }
  }
  duplicateHelper(l1,l2,List())
}

def addElemetsNTimes[A](element: A, times: Int, list: List[A]):List[A]={
  if(times==0)
    return list
  addElemetsNTimes(element, times-1, list:::List(element))
}

//zadanie drugie 2,5p
def isDuplicated[A]( list: List[A],element : A): Boolean = {
   list match{
    case(hd::tl) =>{
      if(hd==element)
        return true
      else isDuplicated(tl, element)
    }
    case List() => return false
  }
}

def duplicate1[A](l1: List[A], l2: List[Int]):List[A]={
  def duplicateHelper(lista1: List[A], lista2: List[Int], lista3: List[A]): List[A]={
    if(lista1.isEmpty || lista2.isEmpty)
      return lista3
    (lista1,lista2) match {
      case (hd::tl,hd2::tl2) => {
        if(isDuplicated(lista3,hd))
          return duplicateHelper(tl,tl2,lista3)
        else return duplicateHelper(tl,tl2,lista3:::addElemetsNTimes(hd,hd2,List()))
      }
    }
  }
  duplicateHelper(l1,l2,List())
}

//zadanie trzecie 5p
trait Debug{
  def debugName(): Unit = {
    print(getClass.getSimpleName)
    
  }
}

//zadanie czwarte 5p
trait DebugVars{
  def debugVars(): Unit ={
    var fields = getClass.getDeclaredFields
    for(f <- fields){
      f.setAccessible(true)
      print("Var: "+ f.getName + ": " + f.getClass.getSimpleName + "(" + f.get(this)+") , ")
    }
  }
}


  def main(args: Array[String]): Unit = {
println(duplicate(List(1,2,3),List(0,3,1,4)))
println(duplicate(List("oh","written","in","the","stars"),List(0,3,1,4,5)))
println(duplicate(List(1,2,3,4),List(1,2,3,4)))
println(duplicate(List(1,2,3),List(0)))
println(duplicate(List(1,2,3),List(10)))
println(duplicate(List(1,2,3,3),List(0,3,1,4)))
println(duplicate1(List(1,2,3,3),List(0,3,1,4)))

var p : Person = new Person("Amr","Shantir",20,'M');
var h : Hour = new Hour (12,45)
println(h.debugName());

println(p.debugVars());

  }

class Person (var name : String , var surname : String ,var age : Int ,var  gendre : Char)extends DebugVars   {
  
}

class Hour (hour : Int , minute : Int)extends Debug{
  
}


}