import java.lang.reflect.Field

trait Debug {
  // zad 3
  // 5 pkt
  def debugName(): Unit = println("Class: " + this.getClass.getName)

  // zad 4
  // 5 pkt
  private def getVals[A](pole: Field): String = {
    pole.setAccessible(true)
    pole.getName+" => "+ pole.getType + ", " + pole.get()
  }

  def debugVars(): Unit = {
    for( i <- 1 to (this.getClass.getDeclaredFields.length-4)){
      println(getVals(this.getClass.getDeclaredFields.array(i)))
      this.getClass.getDeclaredFields.array(i).setAccessible(false)
    }
  }
}
