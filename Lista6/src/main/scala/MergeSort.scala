import ParallelUtilities.parallel

object MergeSort {
  val MAX_THREADS = 4 //ograniczenie wynikające z domniemanej liczby wątków, które obsługuje równocześnie mój procesor

  private def swap[A](arr: Array[A], i: Int, j: Int): Unit = {
    val temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
  }

  private def merge(arr: Array[Int], from: Int, to: Int, middle: Int): Unit = {
    var i = from
    while (i <= middle) {
      if (arr(i) > arr(middle + 1)) {
        swap(arr, i, middle + 1)
        for (j <- middle + 1 until to) {
          if (arr(j) > arr(j + 1))
            swap(arr, j, j + 1)
        }
      }
      i += 1
    }
  }

  private def mergeSort(arr: Array[Int], from: Int, to: Int): Unit = {
    if (to - from != 0) {
      if (to - from == 1) {
        if (arr(from) > arr(to))
          swap(arr, from, to)
      }
      else {
        val middle = (to - from) / 2 + from
        mergeSort(arr, from, middle)
        mergeSort(arr, middle + 1, to)
        merge(arr, from, to, middle)
      }
    }
  }

  private def parMergeSort(arr: Array[Int], from: Int, to: Int, threads: Int): Unit = {
    if (to - from != 0) {
      if (to - from == 1) {
        if (arr(from) > arr(to))
          swap(arr, from, to)
      }
      else {
        val middle = (to - from) / 2 + from
        if(threads < MAX_THREADS){
          parallel(parMergeSort(arr, from, middle, threads * 2), parMergeSort(arr, middle + 1, to, threads * 2))
        }
        else {
          mergeSort(arr, from, middle)
          mergeSort(arr, middle + 1, to)
        }
        merge(arr, from, to, middle)
      }
    }
  }

  def mergeSort(tab: Array[Int]): Unit =
    mergeSort(tab, 0, tab.length - 1)

  def parallelMergeSort(tab: Array[Int]): Unit =
    parMergeSort(tab, 0, tab.length - 1, 1)
}
