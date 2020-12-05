// Zadanie 3 (5 pkt)
object zad03 {
  private trait Debug {
    def debugName(): Unit = println(getClass)
  }

  private class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "Test"
  }

  def main(args: Array[String]): Unit = {
    val p: Point = new Point(3, 4)
    p.debugName() // Prints "class zad03$Point"
  }
}
