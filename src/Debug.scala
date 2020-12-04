import java.lang.reflect.Field

import scala.annotation.tailrec

trait Debug {
  // zad 3
  // 5 pkt
  def debugName(): Unit = println("Class: " + this.getClass.getName)

  // zad 4
  // 5 pkt
  private[this] def getVals[A](f: Field): String = {
    f.setAccessible(true)
    "Var: " + f.getName + " => " + f.getType + ", " + f.get(this)
  }

  def debugVars(): Unit = {
    for (i <- 0 until this.getClass.getDeclaredFields.length) {
      println(getVals(this.getClass.getDeclaredFields.array(i)))
    }
  }

  // zad 4
  // 5 pkt
  def debugGetClassName(): String = this.getClass.getName

  private[this] def getFieldName(f: Field): String = f.getName

  private[this] def getFieldType(f: Field): Class[_] = f.getType

  private[this] def getFieldVal(f: Field): Any = {
    f.setAccessible(true)
    val tmp = f.get(this)
    f.setAccessible(false)
    tmp
  }

  private[this] def getFieldParamsAsArray(f: Field): Array[Any] =
    Array(getFieldName(f), getFieldType(f), getFieldVal(f))

  //return value: Array of Array of Any. Every array inside array describes one variable and consist 3 elements in order:
  // name of field (String), type of field (Class[_]) and value of field (Any)
  def debugGetVarsArray(): Array[Array[Any]] = {
    val arr = new Array[Array[Any]](this.getClass.getDeclaredFields.array.length)
    for (i <- 0 until this.getClass.getDeclaredFields.array.length) {
      arr(i) = getFieldParamsAsArray(this.getClass.getDeclaredFields.array(i))
    }
    arr
  }
}
