import scala.annotation.tailrec

object Functions {
  // Zadanie 1
  private def cumulativeProductRec(numbers: List[Double]): Double = {
    if (numbers == Nil) 1
    else numbers.head * cumulativeProductRec(numbers.tail)
  }

  def cumulativeProduct(numbers: List[Double]): Double = {
    if (numbers == null || numbers == Nil) 0
    else cumulativeProductRec(numbers)
  }

}
