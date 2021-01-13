// Zadanie 5 (5 pkt)
object zad05 {
  private trait Debug {
    def className: String = getClass.toString
    def fieldsInfo: List[String] = {
      var result = List[String]()
      getClass.getDeclaredFields.foreach(field => {
        field.setAccessible(true)
        result = ("Var: " + field.getName + " => " + field.getType + ", " + field.get(this)) :: result
      })
      result.reverse
    }
  }

  private class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "Test"
  }

  def main(args: Array[String]): Unit = {
    val p = new Point(3, 4)
    println(p.className)
    p.fieldsInfo.foreach(println)
    /*
     * Prints:
     * class zad05$Point
     * Var: x => int, 3
     * Var: y => int, 4
     * Var: a => class java.lang.String, Test
     */
  }
}
