object Funkcje extends App {

  //na ten moment udalo mi sie zrobic tylko 1 i 4 zadanie
  //*1
  println("*1\n")
  print("Iloczyn zbioru:")
  def multiply(numbers: List[Int]): Int = {
    val result = numbers.product
    println(result)
    return result
  }
  //Zbiór wejściowy, wprowadzenie "ręczne"
  val m1 = List(1, 2, 3, 4, 5)
  multiply(m1)


  //*2
  def formattedStrings(strings:List[String]) : Unit = {

  }



  //*3
  def isInRange(numbers:List[Int]): Unit = {

  }

  //*4
  println("\n*4\n")
  def numberPower(x:Int, y:Int): Int = {
    def _numberPower(result:Int, y:Int):Int = y match{
      case 0 => result
      case _ => _numberPower(result*x, y-1)
    }
    _numberPower(1, y)
    return _numberPower(1, y)

  }

  val x:Int = 2
  val y:Int = 3
  print(x + " do potęgi " + y + " : ")
  println(numberPower(2, 3))


}
