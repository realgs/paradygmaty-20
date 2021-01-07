
object Tests{

  def main(args: Array[String]): Unit={
    val point = new Point(1, 2)
    point.debugName()
    point.debugVars()
    println(point.returnDebugName())
    println(point.returnDebugVars())
    val point2 = new Point(-1, 5)
    point2.debugName()
    point2.debugVars()
    println(point2.returnDebugName().toString())
    println(point2.returnDebugVars().toString())
    val date = new Date(11, 12, 2020)
    date.debugName()
    date.debugVars()
    println(date.returnDebugName())
    println(date.returnDebugVars())
  }

}
