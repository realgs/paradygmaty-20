
object Tests{

  def main(args: Array[String]): Unit={
    val point = new Point(1, 2)
    point.debugName()
    point.debugVars()
    println(point.returnDebugName().toString())
    println(point.returnDebugVars().toString())
  }

}
