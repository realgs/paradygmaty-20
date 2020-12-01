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
    val tmp = f.getName+" => "+ f.getType + ", " + f.get()
    f.setAccessible(false)
    tmp
  }

  def debugVars(): Unit = {
    for( i <- 1 to (this.getClass.getDeclaredFields.length-4)){
      println(getVals(this.getClass.getDeclaredFields.array(i)))
    }
  }

  // zad 4
  // 5 pkt
  def debugGetClassName(): String = this.getClass.getName

  private[this] def getFieldName(f: Field):String = f.getName
  private[this] def getFieldType(f: Field):String = f.getType.toString
  private[this] def getFieldVal(f: Field):String = {
    f.setAccessible(true)
    val tmp=f.get().toString
    f.setAccessible(false)
    tmp
  }
  private[this] def getFiledParamsAsArray(f: Field):List[String] =
    List(getFieldName(f), getFieldType(f), getFieldVal(f))

  def debugGetVarsList(): List[List[String]] = {
    @tailrec
    def helper(array: Array[Field],acc:List[List[String]], beginIndex:Int, endIndex:Int):List[List[String]] = {
      if(beginIndex>=endIndex) helper(array, getFiledParamsAsArray(array(beginIndex)) +:acc,beginIndex-1,endIndex)
      else acc
    }
    helper(this.getClass.getDeclaredFields,Nil,this.getClass.getDeclaredFields.length-4,1)
  }
}
