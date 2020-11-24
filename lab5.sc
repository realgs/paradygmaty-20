import java.lang.reflect.Field
// Karol Waliszewski

// TODO: Implement better collection for ex. 1 & 2.
// 1)
def duplicate[A](elements: List[A])(counters: List[Int]):List[A] =
 elements match {
   case Nil => Nil
   case _ =>
     counters match {
       case Nil => Nil
       case 0::tail => duplicate(elements.tail)(counters.tail)
       case hd::tail => elements.head :: duplicate(elements)((hd-1)::tail)
     }
 }


duplicate(List(1,2,3,4))(List(1,2,3,4))
duplicate(List(1,2,3))(List(0,2,3,4))
duplicate(List(1,2,3,4))(List(1,2,3))
duplicate(List())(List(1,2,3,4))
duplicate(List(1,2,3,4))(List(0))

// 2)
def duplicate2[A](elements: List[A])(counters: List[Int]):List[A] =
  duplicate(elements.distinct)(counters.distinct)

duplicate2(List(2,1,2,1,1))(List(1,1,2,3,4))


trait Debug {
  // 3)
  def debugName(): Unit = println(this.getClass.getSimpleName)
  // 4)
  def debugVars(): Unit = {
    def printFields(fields: Array[Field]):Unit =
      fields.foreach(field => {
        field.setAccessible(true)
        println("Var: " + field.getName + " => " + field.getType + ", " + field.get(this))
      })

    val fields = this.getClass.getDeclaredFields
    printFields(fields.slice(0, fields.length - 1))
  }

  // 5)
  def getDebugName:String = this.getClass.getSimpleName

  def getDebugFields: Array[Field] = {
    val fields = this.getClass.getDeclaredFields
    fields.slice(0, fields.length - 1)
  }

}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

val p : Point = new Point(3,4)
p.debugName()
p.debugVars()


p.getDebugName
p.getDebugFields