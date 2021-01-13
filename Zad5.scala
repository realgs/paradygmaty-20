//Zadanie 5. (5pkt)
object Zad5 {
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
  }
}
