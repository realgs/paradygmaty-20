import Functions.multiply
import Functions.stringConstants
import Functions.ifInRange
import Functions.power

object Tests {

  def main(args:Array[String]):Unit = {

    // 1. function tests
    println(multiply(List(1, 2, 3)) == 6)
    println(multiply(List(5, 200, -60)) == -60000)
    println(multiply(List(6, 200, 40, 0)) == 0)
    println(multiply(List(16.945, 20, -15.21, 46.8)) == -241238.5092)
    println(multiply(Nil) == 0)

    // 2. function tests
    println(stringConstants(List("Knowledge", "is", "power"), '.', " ") == "Knowledge is power.")
    println(stringConstants(List("Cat", "dog", "hamster"), '?', ", ") == "Cat, dog, hamster?")
    println(stringConstants(List("", "", "", "", "", ""), '%', " - ") == " -  -  -  -  - %")
    println(stringConstants(Nil, 'e', "") == "")

    // 3. function tests
    println(ifInRange(List(0, 1, 2, 3, 4, 5), 0, 5))
    println(ifInRange(List(56, 32.45, 0.001, 0, -99.9, -100.91999, 9089.5), -100.92, 9090))
    println(ifInRange(List(-1.6, -1.6, -1.6, -1.5, -1.50432, -1.55), -1.6, -1.5))
    println(!ifInRange(List(-5, 6, -1.4, 101), -100, 100))

    // 4. function tests
    println(power(2, 2) == 4)
    println(power(42, 0) == 1)
    println(power(-3.587, 3) == -46.15238300300001)
    println(power(6, -3) == 0.004629629629629629)
    println(power(0.2, -3) == 125)

  }

}
