import scala.annotation.tailrec

// Zadanie1
def Product(array:List[Double]) : Double =
{
  if(array == Nil) throw new Exception("`array` cannot be null")
  if(array.tail == Nil) array.head
  else array.head * Product(array.tail)
}

//Product(List()) //rzuca wyjątkiem
Product(List(4)) == 4
Product(List(3,-4)) == -12
Product(List(1,1,1,1,1,1,1,1,1,1)) == 1
Product(List(1,2,3,4,5,6,7,8,9,0)) == 0

// Zadanie2
def Merge(array:List[String], end:String, separator:String) : String =
{
    if(array == Nil) end
    else if(array.tail == Nil) array.head + end
    else  array.head + separator + Merge(array.tail, end, separator)
}

Merge(List(), "", "") == ""
Merge(List("ala", "ma", "kota"), ".", " ") == "ala ma kota."
Merge(List(";", ";", ";"), ";", ";") == ";;;;;;"
Merge(List("ka","jak"), " kajak","") == "kajak kajak"

// Zadanie3
@tailrec
def checkRanges(array:List[Double], x:Double, y:Double) : Boolean =
{
  if(x>y) throw new Exception("`x` cannot be greater then `y`")
  if(array == Nil) throw new Exception("`array` cannot be null")
  else if(array.tail == Nil) array.head >= x && array.head <= y
  else if(array.head >= x && array.head <= y) checkRanges(array.tail,x,y)
  else false
}

//checkRanges(List(), 2, 3) //rzuca wyjątkiem
//checkRanges(List(1),2,1) // rzuca wyjątkiem
checkRanges(List(2, 3), 2, 3) == true
checkRanges(List(2, 3,3,3,3,3,3,3),3, 3) == false
checkRanges(List(2,2,2,2,2,1), 1,2) == true

// Zadanie4
def power(base:Double, exponent:Int) : Double =
{
  if(base == 0 && exponent == 0) throw new Exception("`base` and `exponent` cannot be both 0")
  else if(base == 0 && exponent < 0 )  throw new Exception("You cannot divide by 0")
  else if(base == 0) 0
  else if(exponent == 0) 1
  else if(exponent < 0) 1 / power(base, exponent * -1)
  else base * power(base, exponent-1)
}

//power(0,0)  //rzuca wyjątkiem
//power(0,-4)  //rzuca wyjątkiem
power(0,4) == 0
power(1,4) == 1
power(-1,-3) == -1
