//Zadanie 3. (5pkt)
object Zad3 {
  private trait Debug {
    def debugName(): Unit = println("Class: " + getClass.getName)
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
