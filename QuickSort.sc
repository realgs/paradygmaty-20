def quicksort(tab: Array[Int]): Unit = {
  def choose_pivot[A](tab: Array[A])(m: Int)(n: Int): A =
    tab((m+n)/2)

  def swap[A](tab: Array[A])(i: Int)(j: Int): Unit = {
    val aux = tab(i)
    tab(i) = tab(j)
    tab(j) = aux
  }

  def partition(tab: Array[Int])(l: Int)(r: Int): (Int,Int)= {
    var i = l
    var j = r
    val pivot = choose_pivot(tab)(l)(r)
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

  def quick(tab: Array[Int])(l: Int)(r: Int): Unit =
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

  quick(tab)(0)(tab.length - 1)
}