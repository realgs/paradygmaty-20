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
    "Var: " + f.getName+" => "+ f.getType + ", " + f.get(this)
  }

  def debugVars(): Unit = {
    for( i <- 0 to this.getClass.getDeclaredFields.length-1){
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
    val tmp=f.get(this).toString
    f.setAccessible(false)
    tmp
  }
  private[this] def getFiledParamsAsList(f: Field):List[String] =
    List(getFieldName(f), getFieldType(f), getFieldVal(f))

  //return value: List of Lists of String. Every List of Strings describe one variable and consist 3 elements in order:
  // name of field, type of field and value of field
  def debugGetVarsList(): List[List[String]] = {
    @tailrec
    def helper(array: Array[Field],acc:List[List[String]], beginIndex:Int, endIndex:Int):List[List[String]] = {
      if(beginIndex>=endIndex) helper(array, getFiledParamsAsList(array(beginIndex)) +:acc,beginIndex-1,endIndex)
      else acc
    }
    helper(this.getClass.getDeclaredFields,Nil,this.getClass.getDeclaredFields.length-1,0)
  }
}
