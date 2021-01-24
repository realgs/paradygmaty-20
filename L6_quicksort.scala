import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object L6_quicksort {

  //help method
  def partition(tab: Array[Int])(l: Int)(r: Int): (Int, Int) = {
    var i = l
    var j = r

    def choose_pivot(tab: Array[Int])(m: Int)(n: Int): Int = tab((m + n) / 2)
    val pivot = choose_pivot(tab)(l)(r)

    def swap[A](tab: Array[A])(i: Int)(j: Int): Unit = {
      val aux = tab(i)
      tab(i) = tab(j)
      tab(j) = aux
    }
    while (i <= j) {
      while (tab(i) < pivot) i += 1
      while (pivot < tab(j)) j -= 1
      if (i <= j) {
        swap(tab)(i)(j)
        i += 1
        j -= 1
      }
    }
    (i, j)
  }

  //Not Parallel
  def quicksort(tab: Array[Int]): Unit = {
    def quick(tab: Array[Int])(l: Int)(r: Int): Unit = {
      if (l < r) {
        val (i, j) = partition(tab)(l)(r)
        if (j - l < r - i) {
          quick(tab)(l)(j)
          quick(tab)(i)(r)
        }
        else {
          quick(tab)(i)(r)
          quick(tab)(l)(j)
        }
      }
    }
    quick(tab)(0)(tab.length - 1)
  }

  //Parallel
  def quicksortPar(tab: Array[Int]): Unit = {
    def quickPar(tab: Array[Int])(l: Int)(r: Int): Unit = {
      if (l < r) {
        val (i, j) = partition(tab)(l)(r)
        if (j - l < r - i) {
          Future {quickPar(tab)(l)(j)}
          Future {quickPar(tab)(i)(r)}
        }
        else {
          Future {quickPar(tab)(i)(r)}
          Future {quickPar(tab)(l)(j)}
        }
      }
    }
    quickPar(tab)(0)(tab.length-1)
  }
}


