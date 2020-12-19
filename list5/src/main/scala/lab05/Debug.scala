package lab05

import java.lang.reflect.AnnotatedType

//zadanie 3 (5pkt)
trait Debug {
  def debugName(): Unit = println("Class: " + getClass.getSimpleName)

  //zadanie 4 (5pkt)
  def debugVars(): Unit = {
    for (field <- getClass.getDeclaredFields) {
      field.setAccessible(true)
      println("Var: " + field.getName + "=>" + field.getAnnotatedType + ", " + field.get(this))
      field.setAccessible(false)
    }
  }

  //zadanie 5 (5pkt)
  def debugStringName: String = getClass.getSimpleName

  def debugVarsArray: Array[(String, AnnotatedType, AnyRef)] = getClass.getDeclaredFields.map { field =>
    field.setAccessible(true)
    val varsArray = (field.getName, field.getAnnotatedType, field.get(this))
    field.setAccessible(false)
    varsArray
  }
}

