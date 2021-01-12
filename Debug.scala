package List5

import scala.annotation.tailrec

trait Debug {

  //Zadanie 3(5pkt)
    def debugName(): Unit =
      println("Class: " + getClass.getSimpleName)

  //Zadanie 4(5pkt)
    def debugVars(): Unit = {
      val fields = getClass.getDeclaredFields
      var i = 0
      @tailrec
      def printFields(condition: => Boolean)(body: => Unit): Unit =
        if(condition) {
          body
          printFields(condition)(body)
        }

      printFields(i < fields.size) {
        fields(i).setAccessible(true)
        println("Var: " + fields(i).getName + " => " + fields(i).getType + ", " + fields(i).get(this))
        i += 1
      }
    }
}
