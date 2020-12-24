object Utils {
  def executionTime(executable: () => Any): Long = {
    val start = System.currentTimeMillis()
    executable()
    System.currentTimeMillis() - start
  }
}
