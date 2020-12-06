

trait Debug {
  //Zad 3 (5 pkt)
  def debugName(): Unit = {
    val class_name = this.getClass.getName
    val name = class_name.split("\\$").last
    println("Class: " + name)
  }

  //Zad 4 (5pkt)
  def debugVars(): Unit = {
    val fields = this.getClass.getDeclaredFields
    for (field <- fields) {
      field.setAccessible(true)
      val str = "Var: " + field.getName + " => " + field.getType + ", " + field.get(this)
      println(str)

    }
  }

  //Zad 5 (5 pkt)
  def get_debugName(): String = {
    val name_of_class = this.getClass.getName
    val name_of_atribute = name_of_class.split("\\$").last
    name_of_atribute
  }

  def get_debugVars(): List[(Any, Any, Any)] = {
    var list_all = List.empty[(Any, Any, Any)]
    val fields = this.getClass.getDeclaredFields

    for (field <- fields) {
      field.setAccessible(true)
      val name_of_attribute = field.getName
      val type_of_attribute = field.getType
      val value_of_attribute = field.get(this)
      val field_attribute = Tuple3(name_of_attribute, type_of_attribute, value_of_attribute)
      list_all = field_attribute :: list_all
    }
    list_all
  }

}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}


class Car(brand_car: String, model_car: String, year_car: Int,
          type_car: String, capacity_eng_car: Double) extends Debug {
  var brand: String = brand_car
  var model: String = model_car
  var year: Int = year_car
  var type_of: String = type_car
  var capacity_eng: Double = capacity_eng_car
}


val point = new Point(0, 0)
point.debugName()
point.debugVars()
val name = point.get_debugName()
val attributes = point.get_debugVars()

val car = new Car("Hyundai", "Solaris",
  2020, "Sedan", 1.6)
car.debugName()
car.debugVars()
val name = car.get_debugName()
val attributes = car.get_debugVars()



