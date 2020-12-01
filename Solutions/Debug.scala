package Solutions

trait Debug {
  //ex 3 (5 pkt)
  def debugName = println(s"Class: ${getClass.getSimpleName}")

  //ex 4 (5 pkt)
  def debugVars() = {
    val fields = getClass.getDeclaredFields
    var i = 0
    while(i < fields.length){
      fields(i).setAccessible(true)
      println("Var: " + fields(i).getName + " => " + fields(i).getType + ", " + fields(i).get(this))
      i += 1
    }
  }

  def getName: String = getClass.getSimpleName

  def getVars: Map[String, (Class[_], AnyRef)] = {
    val fields = getClass.getDeclaredFields
    var map: Map[String, (Class[_], AnyRef)] = Map()
    var i = 0
    while(i < fields.length){
      fields(i).setAccessible(true)
      map = map + (fields(i).getName -> (fields(i).getType, fields(i).get(this)))
      i += 1
    }
    map
  }

}
