package Tests
import main.Computations._

object MaxValueTest {

  def runTest(): Unit = {
    test1()
    test2()
  }

  def test1(){
    println("1. Max value of array test")
    val array = Array(34,10,-155,155,2,2,6,-1)
    val maxValue = 155
    println(arrayMaxValue(array) == maxValue)
    println(arrayMaxValueFuture(array) == maxValue)
    println(arrayMaxValueParallel(array) == maxValue)
  }

  def test2(){
    println("2. Max value of array test")
    val array = Array(1,1,1,1,1)
    val maxValue = 1
    println(arrayMaxValue(array) == maxValue)
    println(arrayMaxValueFuture(array) == maxValue)
    println(arrayMaxValueParallel(array) == maxValue)
  }



}
