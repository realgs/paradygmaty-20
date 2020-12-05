package scala.OOP

import scala.collection.immutable.VectorMap

object ExerciseThreeAndFourAndFiveBasicTest {
  def main(args: Array[String]): Unit = {
    val point1 = new Point(3, 4)
    val point2 = new Point(10, 20)
    val user = new User("Jan", "Nowak", 40, point2)

    println("Exercise three: ")
    println("Point instance: ")
    point1.debugName()
    println("User instance")
    user.debugName()

    println("____________________")

    println("Exercise four: ")
    println("Point instance: ")
    point1.debugVars()
    println("User instance")
    user.debugVars()

    println("____________________")

    println("Exercise five: ")
    println("Point instance: ")
    printInfoUsingMapFromExerciseFive(point1.collectDebugVarsAsMap())
    println("User instance")
    printInfoUsingMapFromExerciseFive(user.collectDebugVarsAsMap())
  }

  private def printInfoUsingMapFromExerciseFive(varsMap: VectorMap[String, (Class[_], Any)]): Unit = {
    varsMap.foreach(keyValueTuple => println("Var: " + keyValueTuple._1 + " => " + keyValueTuple._2._1.getTypeName + ", " + keyValueTuple._2._2))
  }
}
