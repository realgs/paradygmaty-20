trait Debug {

  //Zadanie 3
  def debugName() : Unit = {
    println("Class: " + getClass().getName())
  }

  def debugVars() : Unit = {
    val fields = getClass().getDeclaredFields()
    println()
    for( i <- 0 until fields.length)
    {
      val field = fields(i).toString().split(" ")
      fields(i).setAccessible(true)
      println("Var: " + field(2).substring(getClass().getName().length()+1) + " => " + field(1) + ", " + fields(i).get(this))
    }
  }

//  Var: x => int, 3
//  Var: y => int, 4
//  Var: a => java.lang.String, test

  //Zadanie 5

}