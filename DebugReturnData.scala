package L5
import java.lang.reflect.Field
import scala.annotation.tailrec

// zadanie 5 (5 pkt)
trait DebugReturnData {
  def getName(): String = {
    getClass.getSimpleName
  }

  def getName_Type_ValueOfFieldsList(): List[(String, Class[_], Object)] = {
    @tailrec
    def innerFunction (arrayOfFields: Array[Field], iterator: Integer, resultList: List[(String, Class[_], Object)]): List[(String, Class[_], Object)] = {
      if (iterator < arrayOfFields.size) {
        arrayOfFields(iterator).setAccessible(true)
        innerFunction(arrayOfFields, iterator + 1,  (arrayOfFields(iterator).getName, arrayOfFields(iterator).getType, arrayOfFields(iterator).get(this)) :: resultList)
      }
      else resultList.reverse
    }
    innerFunction(getClass.getDeclaredFields, 0, Nil)
  }
}

