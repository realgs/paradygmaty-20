import scala.util.Random

object Test extends App {

  def generateRandomList(n: Int, newList: List[Int]): List[Int] = {
    if (n == 0) newList
    else generateRandomList(n - 1, Random.nextInt(10000) :: newList)
  }
  //czas przy zrównolegleniu będzie zyskany tylko operując na dużych ilościach danych,
  //dlatego w tych testach korzystałam właśnie z takich danych,
  // jednak jeżeli operować na małej ilości danych w zrownoleglionym programie, to jego działanie może być spowolnione

  var numbers = generateRandomList(900000, List()).toArray
  var res1 = numbers
  var res2 = numbers

  var t1 = System.currentTimeMillis()
  QuickSort.quickSortSimple(res1)
  var t2 = System.currentTimeMillis()
  val result1 = t2 - t1
  println("Sorting using simple algorithm was in time  " + result1 + " milliseconds")

  t1 = System.currentTimeMillis()
  QuickSort.quickSortParallel(res2)
  t2 = System.currentTimeMillis()
  val result2 = t2 - t1
  println("Sorting using parallel programming was in time " + result2 + " milliseconds")

  println("Using parallel programming it was " + (result1 - result2) + " milliseconds faster")

  println("-------------------------------------------------------------------------------")

  t1 = System.currentTimeMillis()
  val fib35 = Fibonaci.fibSimple(35)
  t2 = System.currentTimeMillis()
  val result3 = t2 - t1
  println("Getting 35 Fibonaci number using simple algorithm was in time : " + result3 + " milliseconds;" + " 35 number Fibonaci is: " + fib35)

  t1 = System.currentTimeMillis()
  val fib35Par = Fibonaci.fibParallel(35)
  t2 = System.currentTimeMillis()
  val result4 = t2 - t1
  println("Getting 35 Fibonaci number using parallel programming was in time " + result4 + " milliseconds;" + " 35 number Fibonaci is: " + fib35Par)

  println("Using parallel programming it was " + (result3 - result4) + " milliseconds faster")
}
