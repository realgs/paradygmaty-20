import Functions.{multiplyNumbers, power, transformString, withinRange}

object FunctionsTests {
  def main(args: Array[String]): Unit = {
    println(multiplyNumbers(Nil) == 0)
    println(multiplyNumbers(List(2)) == 2)
    println(multiplyNumbers(List(12.5, 0, 11, 10)) == 0)
    println(multiplyNumbers(List(1.5, 2.25, -2.75, 0.5, 4)) == -18.5625)
    println(multiplyNumbers(List(-12345.12345, 23456.23456)) <= -2.895701 * Math.pow(10, 8))

    println(transformString(List(), "?", " ") == "")
    println(transformString(List(""), "...", " ") == "...")
    println(transformString(List("", "", ""), "!", ", ") == ", , !")
    println(transformString(List("This", "is", "test"), ".", " ") == "This is test.")
    println(transformString(List("Unusu", "visu", "ritual"), "!", "al ") == "Unusual visual ritual!")

    println(withinRange(List(), -2, 4))
    println(withinRange(List(-1, 1, 2, 3, 4), -1, 5))
    println(withinRange(List(-125.5, -1.2, 0, 2, 4.25), -125.5, 4.25))
    println(withinRange(List(-1, 0, 1), -1, 1))
    println(withinRange(List(-1, -2, 0, 2), -1, 1), " powinno rzucic false")
//    println(withinRange(List(1,2,3,4), 5, 4)) // exception - illogical range


    println(power(0, 1) == Math.pow(0, 1))
    println(power(0, 0) == Math.pow(0, 0))
    println(power(10, 0) == Math.pow(10, 0))
    println(power(-2, 0) == Math.pow(-2, 0))
    println(power(-2, 9) == Math.pow(-2, 9))
    println(power(2, 9) == Math.pow(2, 9))
    println(power(2.5, 4) == Math.pow(2.5, 4))
    println(power(-1.5, 2) == Math.pow(-1.5, 2))
    println(power(5, -3) == Math.pow(5, -3))

    // dla bardziej szczegółowych wynikow Math.pow nie zadziala wystarczajaco dokladnie bo java nie uzywa BigDecimal w tej metodzie
    // wynik z funkcji uzywajacej BigDecimal jest bardziej dokladny niz Math.pow wiec sprawdzam >=

    println(power(123, 20) >= Math.pow(123, 20))
    println(power(123.1123, 60) >= 2.61870571 * Math.pow(10, 125))
    println(power(123, -20) >= 1.5918339803 * Math.pow(10, -42))
    power(0, -1) // exception
  }
}
