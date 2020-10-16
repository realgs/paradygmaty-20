import scala.annotation.tailrec

def Product(array:List[Double]) : Double =
{
  if(array == Nil) throw new Exception("`array` cannot be null")
  if(array.tail == Nil) array.head
  else array.head * Product(array.tail)
}

def Merge(array:List[String], end:String, separator:String) : String =
{
    if(array == Nil) end
    else if(array.tail == Nil) array.head + end
    else  array.head + separator + Merge(array.tail, end, separator)
}


@tailrec
def checkRanges(array:List[Double], x:Double, y:Double) : Boolean =
{
  if(array == Nil) throw new Exception("`array` cannot be null")
  else if(array.tail == Nil) array.head >= x && array.head <= y
  else if(array.head >= x && array.head <= y) checkRanges(array.tail,x,y)
  else false
}

def power(base:Double, exponent:Int) : Double =
{
  if(base == 0 && exponent == 0) throw new Exception("`base` and `exponent` cannot be both 0")
  else if(base == 0 && exponent < 0 )  throw new Exception("You cannot divide by 0")
  else if(base == 0) 0
  else if(exponent == 0) 1
  else if(exponent < 0) 1 / power(base, exponent * -1)
  else base * power(base, exponent-1)
}