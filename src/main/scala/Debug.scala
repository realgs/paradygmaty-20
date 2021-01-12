
trait Debug {

  // Zad 3 (5pkt)
  def printDebugName(): Unit = {
    println("Class: " + getDebugName)
  }

  // Zad 5 (5pkt)
  def getDebugName: String = getClass.getName

  // Zad 4 (5pkt)
  def printDebugVars(): Unit = {
    for (field <- getClass.getDeclaredFields) {
      field.setAccessible(true)
      println(
        (if (field.getModifiers == 2) "Var: " else "Val: ") +
          field.getName +
          " => " +
          field.getType.getName +
          ", " +
          field.get(this)
      )
    }

  }

  // Zad 5 (5pkt)
  // Boolean represents whether field is modifiable (val or var)
  def getDebugVars: Map[String, (Any, Boolean)] = {
    val fields = getClass.getDeclaredFields
    fields.foreach(_.setAccessible(true))
    fields.foldLeft(Map[String, (Any, Boolean)]())((map, field) => map + (field.getName -> (field.get(this), field.getModifiers != 2)))
  }

}
