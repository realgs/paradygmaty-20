object Utils {
  def executionTime(executable: () => Any): Long = {
    val start = System.currentTimeMillis()
    executable()
    System.currentTimeMillis() - start
  }

  def isSorted(array: Array[Int]): Boolean = {
    var sorted = true
    var i = 0
    while (sorted && i < array.length - 1) {
      sorted = array(i) <= array(i + 1)
      i += 1
    }
    sorted
  }
}
