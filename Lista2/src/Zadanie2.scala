object Zadanie2 extends App{
  def stringBuilder(list: List[String], classifier: String, end: String): String =
    if(list==Nil) end
    else if(list.tail!=Nil) list.head+classifier+stringBuilder(list.tail, classifier, end)
    else list.head+end

  println(stringBuilder(List("My","name","is","Klaudia")," ^ ",".")=="My ^ name ^ is ^ Klaudia.")
  println(stringBuilder(List("123","456", "789"), "-", "!")=="123-456-789!")
  println(stringBuilder(List(), "&", ".")==".")
  println(stringBuilder(List(" "," ", " "), "&", ".")==" & & .")
}
