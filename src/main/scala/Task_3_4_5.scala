object Task_3_4_5 {
  trait Debug {
    //zadanie 3 (5 pkt)
    def debugName(): Unit = println("Class: " + getClass.getSimpleName)

    //zadanie 4 (5 pkt)
    def debugVars(): Unit = {
      getClass.getDeclaredFields.foreach(f => {
        f.setAccessible(true)
        println("Var: " + f.getName + " => " + f.getType.getName + "," + f.get(this))
      })
    }

    //zadanie 5 (5 pkt)
    def getNameOfClass(): String = "Class: " + getClass.getSimpleName

    def getListWithVarsInfo(): List[String] = {
      var listOfVarsInfo:List[String] = List()
      getClass.getDeclaredFields.foreach(f => {
        f.setAccessible(true)
        listOfVarsInfo = listOfVarsInfo :+ ("Var: " + f.getName + " => " + f.getType.getName + "," + f.get(this))
      })

      listOfVarsInfo
    }
  }
}
