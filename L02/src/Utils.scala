object Utils {
  def assertThatExceptionIsThrown(executable: () => Unit): Unit = {
    assert(try {
      executable()
      false
    } catch {
      case e: Exception => true
    })
  }
}
