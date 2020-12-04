package L5
import scala.annotation.tailrec
import java.lang.reflect.Field

trait Debug {
  // zadanie 3 (5 pkt)
  def debugName(): Unit =
    println ("Class: " + getClass.getSimpleName)

  // zadanie 4 (5 pkt)
  def debugVars(): Unit = {
    @tailrec
    def innerFunction (arrayOfFields: Array[Field], iterator: Integer): Unit =
      if (iterator < arrayOfFields.size) {
        arrayOfFields(iterator).setAccessible(true)
        println("Var: " + arrayOfFields(iterator).getName + " => " + arrayOfFields(iterator).getType + ", " + arrayOfFields(iterator).get(this))
        innerFunction(arrayOfFields, iterator + 1)
      }
    innerFunction(getClass.getDeclaredFields, 0)
  }
}

