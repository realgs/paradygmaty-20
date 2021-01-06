// Adrian Chłopowiec
package algorithms

import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.CollectionConverters._

object MatrixMultiplication
{
  def multiply(left: Array[Array[Int]], right: Array[Array[Int]]): Array[Array[Int]] =
    {
      if(left(0).length != right.length)
        return Array()

      val product = new ArrayBuffer[ArrayBuffer[Int]]()
      for(m <- left.indices)
        {
          product.addOne(new ArrayBuffer[Int]())
          for(p <- right(0).indices)
            {
              var elem = 0
              for(n <- left(0).indices)
              {
                elem += left(m)(n) * right(n)(p)
              }
              product(m).addOne(elem)
            }
        }
      val newMatrix = product.map(arr => arr.toArray[Int])
      newMatrix.toArray
    }
}

object MatrixMultiplicationPar
{
  def multiplyPar(left: Array[Array[Int]], right: Array[Array[Int]]): Array[Array[Int]] =
    {
      left.par.map(arr => multiply(arr, right)).toArray
    }

  private def multiply(left: Array[Int], right: Array[Array[Int]]): Array[Int] =
  {
    val product = new ArrayBuffer[Int]()
    for(p <- right(0).indices)
    {
      var elem = 0
      for(n <- left.indices)
      {
        elem += left(n) * right(n)(p)
      }
      product.addOne(elem)
    }
    product.toArray
  }
}
