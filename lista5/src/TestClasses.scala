import Functions_L5.Debug

object TestClasses {

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  class Point3D(xv: Int, yv: Int, zv: Int) extends Point(xv, yv) {
    var z: Int = zv
  }

  class Point4D(xv: Int, yv: Int, zv: Int, vv: Int) extends Point3D(xv, yv, zv) {
    var v: Int = vv
  }

  class Animal(namev: String, agev: Int) extends Debug {
    var name: String = namev
    var age: Int = agev
  }

  class Dog(namev: String, agev: Int, breadv: String) extends Animal(namev, agev) {
    var bread: String = breadv
  }
}
