import l2.{intoSentence, isBetween, multiply, power}

object l2_test {
  def main(args: Array[String]): Unit ={

    println("Zadanie 1")
    println(multiply(List()))
    println(multiply(List(1, 2, 3, 4, 5)))
    println(multiply(List(-0.1, 4, -0.25, 3, 2, -5)))

    println("Zadanie 2")
    println(intoSentence(List("Ala","ma","kota"),'.',' '))
    println(intoSentence(List("Is","sentence","creator","working"),'?','_'))
    println(intoSentence(List(), '.', ' '))

    println("Zadanie 3")
    println(isBetween(List(2.1, 3, 7), 0, 5))
    println(isBetween(List(2, 2.2, 2.6, 2.8, 3), 2, 3))
    println(isBetween(List(), 3, 3.5))
    println(isBetween(List(2, 2.5, 3, 3.5), 3, 0))

    println("Zadanie 4")
    println(power(1.5, 2))
    println(power(0.25, -2))
    println(power(2, 10))
    println(power(0, 9))
    println(power(13, 0))

  }

}
