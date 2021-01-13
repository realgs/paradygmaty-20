//Zadanie 4. (5pkt)
object Zad4 {
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

  }
}
