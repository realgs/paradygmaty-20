object Tests_L2 {
  //testy napisane w sposob w jaki robimy to na cwiczeniach
  def main(args: Array[String]): Unit = {
    listMultiplication_Test()
    sentenceCreator_Test()
    numbersInRange_Test()
    pow_Test()
  }

  def listMultiplication_Test(): Unit = {
    println("First function tests")
    println(Functions_L2.listMultiplication(List(1, 2, 3, 4, 5)) == 120)
    println(Functions_L2.listMultiplication(List()) == 1)
    println(Functions_L2.listMultiplication(List(1.5, 2.5, 5.5)) == 20.625)
    println(Functions_L2.listMultiplication(List(-1, 7, -3, -2)) == -42)
    println(Functions_L2.listMultiplication(List(1, 2, 3, 4, 0)) == 0)
  }

  def sentenceCreator_Test(): Unit = {
    println("\nSecond function tests")
    println(Functions_L2.sentenceCreator(List("programming", "is", "awesome"), '!', ' ') == "programming is awesome!")
    println(Functions_L2.sentenceCreator(List(), '!', ' ') == "")
    println(Functions_L2.sentenceCreator(List("1", "2", "3", "4"), '.', ',') == "1,2,3,4.")
    println(Functions_L2.sentenceCreator(List("mouse"), '?', ' ') == "mouse?")
  }

  def numbersInRange_Test(): Unit = {
    println("\nThird function tests")
    println(Functions_L2.numbersInRange(List(1, 5, 2, 7), 1, 7) == true)
    println(Functions_L2.numbersInRange(List(-1, 19, -20, 7), 0, 30) == false)
    println(Functions_L2.numbersInRange(List(10), 10, 10) == true)
    //exception thrown
    //println(Functions_L2.numbersInRange(List(), 10, 10))
    //exception thrown
    //println(Functions_L2.numbersInRange(List(10, 11, 18, -10, 0), 100, 10))
  }

  def pow_Test(): Unit = {
    println("\nFourth function tests")
    println(Functions_L2.pow(10, 0) == 1)
    println(Functions_L2.pow(10, 3) == 1000)
    println(Functions_L2.pow(10, -3) == 0.001)
    println(Functions_L2.pow(-5, 2) == 25)
    println(Functions_L2.pow(-5, 3) == -125)
    //exception thrown
    //println(Functions_L2.pow(0, 0))
  }
}
