package Source


import scala.collection.mutable.Map

trait Debug {

  // Zadanie 3 (5 pkt)
  def debugName(): Unit =
  {
    println(s"Class: ${getClass.getSimpleName}")
  }

  // Zadanie 4 (5 pkt)
  def debugVars(): Unit =
  {
      val vars = getClass.getDeclaredFields
      for (i <- 0 until vars.length)
      {
        val field = vars(i)
        field.setAccessible(true)
        println(s"Var: ${field.getName} => ${field.getType}, ${field.get(this)}")
      }
  }

  // Zadanie 5 (5 pkt)
  def getClassName: (String, String) =
  {
    ("Class", getClass.getSimpleName)
  }

  def getVars: Map[String, (Class[_], Any)] =
  {
    val dict = Map[String, (Class[_], Any)]();
    val vars = getClass.getDeclaredFields
    for (i <- 0 until vars.length)
    {
      val field = vars(i)
      field.setAccessible(true)
      dict.put(field.getName, (field.getType, field.get(this)))
    }
    dict
  }
}
