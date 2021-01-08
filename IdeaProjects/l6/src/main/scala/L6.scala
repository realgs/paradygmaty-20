import scala.annotation.tailrec

object L6 {

  def measureTime[T](fun: =>T): Long={
    val startTime=System.nanoTime()
    val result=fun
    val endTime=System.nanoTime()
    (endTime-startTime)
  }

  def main(args: Array[String])={
    System.out.println(BinarySearch.contains(Array(1, 2, 3, 4, 5, 6, -1, 22, 33, 41, 1233), -1))
    System.out.println(BinarySearch.contains(Array(1, 2, 3, 4, 5, 6, -11, 22, 33, 41, 1233), -1))
    System.out.println(BinarySearch.contains(Array(), -1))
  }
}
