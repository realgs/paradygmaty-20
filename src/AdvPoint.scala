import Solutions.Debug

class AdvPoint(xv: Int, yv: Int, minBound: Int, maxBound: Int) extends Debug {
  private var _x = xv
  private var _y = yv
  private val bounds = (minBound, maxBound)

  def x: Int = _x

  def x_=(newValue: Int): Unit = {
    if (newValue > bounds._1 && newValue < bounds._2) _x = newValue else printWarning()
  }

  def y: Int = _y

  def y_=(newValue: Int): Unit = {
    if (newValue > bounds._1 && newValue < bounds._2) _y = newValue else printWarning()
  }

  private def printWarning(): Unit = println("WARNING: Out of bounds")
}

