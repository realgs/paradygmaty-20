// Zadanie 1
def makeProduct(list: List[Double]): Double =
{
  if(list == Nil) 0
  else if(list.tail == Nil) list.head
  else list.head * makeProduct(list.tail)
}

makeProduct(List()) == 0
makeProduct(List(1, 2, 3)) == 6
makeProduct(List(1.5, 1.5)) == 2.25
makeProduct(List(1.5, 2.3, 5.1, 0)) == 0
makeProduct(List(-2, 3, -0.5, -4)) == -12

// Zadanie 2
def fancyConcat(strings: List[String], terminator: String, separator: String): String =
  if(strings == Nil) ""
  else if(strings.tail == Nil) strings.head + terminator
  else strings.head + separator + fancyConcat(strings.tail, terminator, separator)

fancyConcat(List("Wiadomo", "kto", "wiadomo", "co", "wiadomo",
  "ile", "wiadomo", "bez", "czego"), "...", " ") ==
  "Wiadomo kto wiadomo co wiadomo ile wiadomo bez czego..."

fancyConcat(List("Hail", "to", " ", "the", "king"), "!", "#") ==
  "Hail#to# #the#king!"

fancyConcat(List("a", "b"), "c", "") == "abc"

// Zadanie 3
@scala.annotation.tailrec
def checkRange(list: List[Double], lower: Double, higher: Double): Boolean =
  if(list == Nil) false
  else if(list.tail == Nil && (list.head <= higher && list.head >= lower)) true
  else if(list.head <= higher && list.head >= lower) checkRange(list.tail, lower, higher)
  else false

checkRange(List(1, 2, 3, 4), 0, 5) == true
checkRange(List(1, 5, 10), 1, 10) == true
checkRange(List(-10, 10, 50), 0, 100.25) == false
checkRange(List(10.12, 5.32, 20.1), 5.32,20.1) == true
checkRange(List(100, 10, 50), 5, 99) == false

// Zadanie 4
def power(base: Double, exponent: Int): Double =
{
  if(base == 0 && exponent < 0) throw new IllegalArgumentException()
  if(exponent == 0) return 1
  @scala.annotation.tailrec
  def powerIterPos(accum: Double, i: Int): Double =
      if(i == exponent) accum
      else powerIterPos(accum * base, i + 1)

  @scala.annotation.tailrec
  def powerIterNeg(accum: Double, i:Int): Double =
    {
      if(i == exponent) accum
      else powerIterNeg(accum * (1 / base), i - 1)
    }
  if(exponent > 0)
    powerIterPos(base, 1)
  else
    powerIterNeg(base, 1)
}

val epsilon = 1.0e-6
power(2, 4) == 16
power(12, 0) == 1
power(3, 3) == 27
power(6, 1) == 6
power(0, 5) == 0
math.abs(power(3, -1) - 1.0/3) <= epsilon
math.abs(power(2, -2) - 1.0/4) <= epsilon
math.abs(power(5, -3) - 1.0/125) <= epsilon
