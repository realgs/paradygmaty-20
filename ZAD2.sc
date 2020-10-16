object Zad2 {
  def main(list:List[String], seporator: String,znak:String): String = {
    if(list != Nil){
      if(list.length == 1){
       return list.head+znak + main(list.tail, seporator, znak)
      }
      else {
        return list.head + seporator + main(list.tail, seporator, znak)
      }
      }
    else{
      return "";
    }
  }
}


val polaczenie = ""
val arr_1 = List("Hleb","Liaonik","Zad 2")
val arr_2 = List("a","b","c","d","e")
val arr_3 = List("I","love","paradigms")

print(Zad2.main(arr_1, " ", "?"))
print(Zad2.main(arr_2, "/","."))
print(Zad2.main(arr_3, " | ","!"))
