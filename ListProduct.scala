package L6
import ParallelComputing.parallel

object ListProduct {
  final val threshold = 2000
  private def multiplyElements(list: List[Int]):Int = {
    var product = 1
    for (i <- 0 until list.length)
      product = product * list(i)
    product
  }

  def listProduct (list: List[Int]): Int = {
    if (list.length == 0) 0
    else multiplyElements(list)
  }

  def listProductParallel (list: List[Int]): Int = {
    if (list.length == 0) 0
    else {
      val (leftSubList, rightSubList) = list.splitAt(list.length / 2)
      if (list.length > threshold) {
        val (product1, product2) = parallel(listProductParallel(leftSubList), listProductParallel(rightSubList))
        product1 * product2
      }
      else {
        val (product1, product2) = parallel(multiplyElements(leftSubList), multiplyElements(rightSubList))
        product1 * product2
      }
    }
  }
}

