trait Debug {
  def debugName: String = getClass.getSimpleName

  def debugVars: Array[(String,Class[_],Object)] = {
    val fields = getClass.getDeclaredFields
    val infoAboutFields = new Array[(String,Class[_],Object)](fields.size)
    for (i <- 0 until infoAboutFields.size) {
      fields(i).setAccessible(true)
      infoAboutFields(i) = (fields(i).getName,fields(i).getType,fields(i).get(this))
    }
    infoAboutFields
  }
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

var p : Point = new Point(3,4)
p.debugName
p.debugVars;

