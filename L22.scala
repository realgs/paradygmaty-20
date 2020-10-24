object L22 extends App {

  def zadanie1 (x: List[Double]): Double =
    {
      if (x == Nil) 1
      else x.head * zadanie1(x.tail)
    }

  println("printing true for valid tests, false for invalid:")
  println(zadanie1(List(1.0,2.0,3.0,4.0))==24.0)
  println(zadanie1(List(3.0,2.0,3.0,4.0))==72.0)
  println(zadanie1(List(1.0,0.0,4.0))==0.0)
  println(zadanie1(List(0.0))==0.0)
  println("\n")

  def zadanie2 (l: List[String], char_end: Char, separator: Char): String =
    {
      if (l.length == 1) l.head + char_end
      else l.head + separator + zadanie2(l.tail, char_end, separator)
    }

  println("printing true for valid tests, false for invalid:")
  println(zadanie2(List("I", "love", "cookies"), '!', ' ')=="I love cookies!")
  println(zadanie2(List("numbers in order ={", "1 ", "2 ", "3 ", "4 "), '}', ' ')=="numbers in order ={ 1  2  3  4 }")
  println(zadanie2(List("letters in order ={", "a", "b", "c", "d", "e"), '}', ',')=="letters in order ={,a,b,c,d,e}")
  println("\n")

  def zadanie3 (l: List[Double], X: Double, Y: Double): Boolean =
  {
    if (l==Nil || X>=Y) println ("error, wrong imput")
    else for (i <- l)
      {
        if (i<X || i>Y) return false
      }
    true
  }

  println("printing true for valid tests, false for invalid:")
  println(zadanie3(List(1,4,2,5,3,1), -1, 9))
  println(zadanie3(List(0), -1, 1))
  println(zadanie3(List(9,8,7,6,5), -100, 9))
  println("\n")

  def zadanie4 (X: Double, Y: Double): Double =
  {
  //  if (Y==0) 1
    if (Y>0) X*zadanie4(X,Y-1)
    else if (Y<0) 1/X*zadanie4(X,Y+1)
    else 1.0
  }

  println("printing true for valid tests, false for invalid:")
  println(zadanie4(-1, 9) == -1)
  println(zadanie4(2, 4)==16)
  println(zadanie4(0, 8)==0)
  println(zadanie4(2, -3)==0.125)
  println("\n")


}