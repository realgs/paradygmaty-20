package fourthFifthTasks

object Operators {
  val + : (Double, Double) => Double = (a1, a2) => a1 + a2
  val - : (Double, Double) => Double = (a1, a2) => a1 - a2
  val * : (Double, Double) => Double = (a1, a2) => a1 * a2
  val / : (Double, Double) => Double = (a1, a2) => if (a2 != 0) a1 / a2 else throw new Exception("division by 0 is forbidden!")
}
