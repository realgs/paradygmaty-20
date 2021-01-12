package scala.OOP

import scala.collection.immutable.VectorMap

trait Debug {
  // zad3 (5pkt)
  def debugName(): Unit = println("Class: " + getClass.getSimpleName)

  // zad4 (5pkt)
  def debugVars(): Unit = getClass.getDeclaredFields.foreach(field => {
    field.setAccessible(true)
    println("Var: " + field.getName + " => " + field.getType.getName + ", " + field.get(this))
  }
  )

  // zad5 (5pkt)
  def collectClassNameAsString(): String = getClass.getSimpleName

  def collectDebugVarsAsMap(): VectorMap[String, (Class[_], Any)] =
    getClass.getDeclaredFields.foldLeft(VectorMap[String, (Class[_], Any)]())((map, field) => {
      field.setAccessible(true)
      map + ((field.getName, (field.getType, field.get(this))))
    })
}
