object Debugging {
  //klasa do testowania
  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  trait Debug{
    //Zad 3 (5 pkt)
    def debugName(): Unit = println("Class: " + getClass.getSimpleName)

    //zad 4 (5 pkt)
    def debugVars(): Unit = {
      for (field <- getClass.getDeclaredFields) {
        field.setAccessible(true)
        println("Var: " + field.getName + " => " + field.getType + ", " + field.get(this))
      }
    }
    
  }

}
