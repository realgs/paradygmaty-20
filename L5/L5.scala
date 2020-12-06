
trait Debug {
//zad 3(5 pkt)
  def debugName(): Unit = println("ClassName: " + getClass.getSimpleName)

//zad 4 (5 pkt)
  def debugVars(): Unit = {
    val classFields = getClass.getDeclaredFields
    for (field <- classFields) {
      field.setAccessible(true)
      println("FieldName: " + field.getName + " FieldType: " + field.getAnnotatedType + " FieldValue: " + field.get(this))
    }
  }

//zad 5 (5 pkt)
  def getDebugName(): String =
    getClass.getSimpleName

  def getDebugVars(): Array[(String,AnyRef,AnyRef)]= {
      val classFields = getClass.getDeclaredFields
      val classFieldsInfo = new  Array[(String,AnyRef,AnyRef)](classFields.length)
      var i = 0
        for(field <- classFields) {
          field.setAccessible(true)
          classFieldsInfo(i) = (field.getName,field.getAnnotatedType,field.get(this))
          i+=1
        }
      return classFieldsInfo
    }
}
//test
  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }
class Testo(xv: String, yv: Double) extends Debug {
  var x: String = xv
  var y: Double = yv
  var a: Int = 123
}

val point = new Point(0, 0)
println("func from task 3 and 4")
point.debugName()
point.debugVars()
println("func from task 5")
point.getDebugName()
point.getDebugVars()

val testo = new Testo("Testo",123)
println("func from task 3 and 4")
testo.debugName()
testo.debugVars()
println("func from task 5")
testo.getDebugName()
testo.getDebugVars()