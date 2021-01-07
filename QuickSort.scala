package L6
import ParallelComputing.parallel

object QuickSort {
  final val threshold = 1000

  private def swap (tab: Array[Double]) (i: Int) (j: Int): Unit = {
    val aux = tab(i)
    tab(i) = tab(j)
    tab(j) = aux
  }

  private def partition (tab: Array[Double]) (l: Int) (r: Int): (Int, Int) = {
    var i = l
    var j = r
    val pivot = tab((l + r)/2)
    while (i <= j) {
      while (tab(i) < pivot)
        i+=1
      while (pivot < tab(j))
        j-=1
      if (i <= j) {
        swap (tab)(i)(j)
        i+=1
        j-=1
      }
    }
    (i, j)
  }

  // sequential version
  private def quick (tab: Array[Double]) (l: Int) (r: Int): Unit = {
    if (l < r) {
      val (i, j) = partition (tab)(l)(r)
      if (j - l < r - i) {  // speedup for sequential version
        quick (tab)(l)(j)
        quick (tab)(i)(r)
      }
      else {
        quick (tab)(i)(r)
        quick (tab)(l)(j)
      }
    }
  }

  private def quickParallel (tab: Array[Double]) (l: Int) (r: Int): Unit = {
    if (l < r) {
      val (i, j) = partition (tab)(l)(r)
      if (j - l > threshold && r - i > threshold) parallel(quickParallel (tab)(l)(j), quickParallel (tab)(i)(r))
      else parallel (quick (tab)(l)(j), quick (tab)(i)(r))
    }
  }

  def quickSort(tab: Array[Double]): Unit =
    quick (tab)(0)(tab.length -1)

  def quickSortParallel(tab: Array[Double]): Unit =
    quickParallel (tab)(0)(tab.length -1)
}

