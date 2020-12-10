import java.lang.reflect.AnnotatedElement

trait Debug {

  //Zadanie 3
  def debugName() : Unit = {
    println("Class: " + getClass().getName())
  }

  def debugVars() : Unit = {
    val fields = getClass().getDeclaredFields()
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
  def returnDebugName() : String={
    "Class: " + getClass().getName()
  }

  def returnDebugVars() : List[(String, Class[_], Any)]={
    var l=List[(String, Class[_], Any)]();
    val fields = getClass().getDeclaredFields()
    for( i <- 0 until fields.length) {
    {
      val field = fields(i).toString().split(" ")
      fields(i).setAccessible(true)
      println(fields(i).get(this).getClass())
      l=((field(2).substring(getClass().getName().length()+1)).toString ,fields(i).get(this).getClass(), fields(i).get(this))::l
    }
    }
    l.reverse
  }

}