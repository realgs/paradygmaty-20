//zad4 (5pkt)

trait Debug {
  def debugVars(): Unit = {
    val fields = getClass.getDeclaredFields
    for (i <- 0 until fields.size) {
      fields(i).setAccessible(true)
      println(s"Var: ${fields(i).getName} => ${fields(i).getType}, ${fields(i).get(this)}")
    }
  }
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

var p : Point = new Point(3,4)
p.debugVars;