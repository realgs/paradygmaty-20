object TestPrints_3_4 {
  def main(args: Array[String]): Unit = {

    val p: Point = new Point(3, 4)
    p.debugName()
    p.debugVars()

    val p2: Point = new Point(11, 22)
    p2.debugName()
    p2.debugVars()

  }

}
