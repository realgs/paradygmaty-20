import scala.annotation.tailrec

def myReverse(inputList:List[Int]):List[Int] = {
  @tailrec
  def myReverseHelp(accum: List[Int], inputList: List[Int]): List[Int] =
    inputList match {
      case Nil => accum
      case h :: t => myReverseHelp(h :: accum, t)
    }
  myReverseHelp(Nil,inputList)
}

def podziel(inputList:List[Int]):(List[Int],List[Int]) = {
  @tailrec
  def podzielHelper(inputList:List[Int],accum:(List[Int],List[Int])): (List[Int],List[Int]) =
    inputList match {
      case Nil => accum
      case h :: t =>  if ((h < 0) && (math.abs(h%2) == 1)) podzielHelper(t,(h :: accum._1,h :: accum._2))
                      else if (h < 0) podzielHelper(t,(h :: accum._1,accum._2))
                      else podzielHelper(t,accum)
    }
  val (x,y) = podzielHelper(inputList,(List(),List()))
  (myReverse(x), myReverse(y))
}

podziel(List(-3,-6,8,-9,13))
podziel(List())
podziel(List(1,2,3,4,5,6,7,8,9))
podziel(List(-1,-2,-3,-4,-5,-6,-7,-8,-9))
