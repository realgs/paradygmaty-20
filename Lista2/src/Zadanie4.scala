object Zadanie4 extends App{
  def power(base: Double, exponent: Int): Double =
    if(exponent==0) 1
    else if(exponent>0) base*power(base,exponent-1)
    else (1/base)*power(base, exponent+1)

  println(power(3, 4)==81)
  println(power(4.4,0)==1)
  println(power(2.5,4)==39.0625)
  println(power(-2, 5)==(-32))
  println(power(4, -2)==0.0625)
}
