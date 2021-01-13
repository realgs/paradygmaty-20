import scala.collection.mutable

trait Debug {
  //Zad 3 Punkty: 5
  def debugName(): Unit = println(s"Class: ${getClass.getSimpleName}")

  //Zad 4 Punkty: 5
  def debugVars(): Unit = {
    for (field <- getClass.getDeclaredFields) {
      field.setAccessible(true)
      printf("Var: %s => %s, %s%n", field.getName, field.getType.getSimpleName, field.get(this))
    }
  }

  //Zad 5 Punkty: 5
  def debugNameStr(): String = getClass.getSimpleName

  def debugVarsMap(): mutable.Map[String, (Class[_], AnyRef)] = {
    var map = mutable.Map[String, (Class[_], AnyRef)]()
    for (field <- getClass.getDeclaredFields) {
      field.setAccessible(true)
      map += (field.getName -> (field.getType, field.get(this)))
    }
    map
  }

}
