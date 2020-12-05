// Zadanie 4 (5 pkt)
object zad04 {
  private trait Debug {
    def debugVars(): Unit = {
      getClass.getDeclaredFields.foreach(field => {
        field.setAccessible(true)
        println("Var: " + field.getName + " => " + field.getType + ", " + field.get(this))
      })
    }
  }

  private class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "Test"
  }

  def main(args: Array[String]): Unit = {
    val p: Point = new Point(3, 4)
    p.debugVars()
    /*
     * Prints:
     * Var: x => int, 3
     * Var: y => int, 4
     * Var: a => class java.lang.String, Test
     */
  }
}
