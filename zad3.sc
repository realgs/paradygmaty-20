//zad3 (5pkt)

trait Debug {
  def debugName = println(s"Class: ${getClass.getSimpleName}")
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

var p : Point = new Point(3,4)
p.debugName;

