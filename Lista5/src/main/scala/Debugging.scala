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

    //Zad 5 (5 pkt)
    def getDebugName: String = getClass.getSimpleName

    def getDebugVars: Array[(String, Class[_], Object)] = {
      val result: Array[(String, Class[_], Object)] = new Array[(String, Class[_], Object)](getClass.getDeclaredFields.length)
      var i = 0
      for (field <- getClass.getDeclaredFields) {
        field.setAccessible(true)
        result(i) = (field.getName, field.getType, field.get(this))
        i += 1
      }
      result
    }

    //Pozyskaną za pomocą tej metody tablicę wynikową łatwiej testować
    def getDebugVarsString: Array[(String, String, String)] = {
      val result: Array[(String, String, String)] = new Array[(String, String, String)](getClass.getDeclaredFields.length)
      var i = 0
      for (field <- getClass.getDeclaredFields) {
        field.setAccessible(true)
        result(i) = (field.getName, field.getType.toString, field.get(this).toString)
        i += 1
      }
      result
    }
  }

}
