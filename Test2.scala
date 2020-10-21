object Test2 extends App {

//Zadania 1
  def iloczyn(array:List[Int]):Int={
      if (array == List()) 1 else array.head * iloczyn(array.tail);

  }

  //Zadania 2
  /*def zdanie(array:List[String], sign:String, space:String):List[String] = {
    if(array != Nil) array.head :: space :: Nil else array.head :: sign :: Nil;
  }*/

  def zdanie2(array:List[String], sign:String, space:String):String = {
    if(array != Nil) array.head + space + zdanie2(array.tail, sign, space) else sign;
  }
  
  //Zadania 3
  def interval(array:List[Int], x:Int, y:Int):Boolean ={
    if(array != Nil) {
      if (array.head < x || array.head > y) false else interval(array.tail, x, y);
    }else{
      true;
    }
  }

  //Zadania 4
  def expo(x:Int,y:Int): Int ={
    if (y >= 1) x * expo(x,y-1) else 1;
  }

println("Zadania 1")
  val number = List(3, 2, 10, -14);
  val number2 = List(3, 2, 10);
  val number3 = List();
  println(iloczyn(number));
  println(iloczyn(number2));
  println(iloczyn(number3));


  println("Zadania 2")
   val sentence = List("Ivan", "drinks", "wine", "with", "cheese");
   val sentence2 = List(" ");
   val sentence3 = List();
   println(zdanie2(sentence, "!", " "));
   println(zdanie2(sentence2, "", " "));
   println(zdanie2(sentence3, "?", ""));


   println("Zadania 3");
   println(interval(number, 1,11));
   println(interval(number, -15, 10));
   println(interval(number3, 1, 11));

  println("Zadania 4")
  println(expo(3, 4));
  println(expo(0, 3));
  println(expo(-2, 3));
  println(expo(-2, 0));
  println(expo(0, 0));

}
