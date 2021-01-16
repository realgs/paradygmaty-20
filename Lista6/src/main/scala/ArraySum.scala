import ParallelUtilities.parallel

object ArraySum {
  val MAX_THREADS = 4 //ograniczenie wynikające z domniemanej liczby wątków, które obsługuje równocześnie mój procesor

  private def sumFromTo(array: Array[Int], from: Int, to: Int): Int = {
    var sum: Int = 0
    for (i <- from until to){
      sum = sum + array(i)
    }
    sum
  }

  def sum(array: Array[Int]): Int = {
    if (array.isEmpty) 0
    else sumFromTo(array, 0, array.length)
  }

  private def parallelSumFromTo(array: Array[Int], from : Int, to: Int, threads: Int): Int = {
    if (array.isEmpty) 0
    else  {
      if(threads < MAX_THREADS){
        val sums = parallel(parallelSumFromTo(array, from, (to - from) / 2 + from, threads * 2), parallelSumFromTo(array, (to - from) / 2 + from, to, threads * 2))
        sums._1 + sums._2
      }
      else{
        sumFromTo(array, from, to)
      }
    }
  }

  def parallelSum(array: Array[Int]): Int = {
    if (array.isEmpty) 0
    parallelSumFromTo(array, 0, array.length, 1)
  }
}