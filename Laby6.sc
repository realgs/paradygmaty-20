import java.util.concurrent._
import org.scalameter._

val forkJoinPool = new ForkJoinPool

def schedule[T](body: => T): ForkJoinTask[T] = {
  val t = new RecursiveTask[T] {
    def compute = body
  }
  Thread.currentThread match {
    case wt: ForkJoinWorkerThread =>
      t.fork()
    case _ =>
      forkJoinPool.execute(t)
  }
  t
}

def task[T](body: =>T):ForkJoinTask[T]={
  schedule(body)
}

def parallel[A,B](taskA: =>A, taskB: =>B):(A,B)={
  val right = task{
    taskB
  }
  val left = taskA
  (left,right.join())
}

//problem 1 : sprawdzenie czy liczba n, jest liczba pierwsza
def check(a: Int, b: Int):Boolean = {
  var l = a
  var m = b
  while(math.pow(l,2)<=m){
    if(m%l==0) return false
    l=l+1
  }
  return true
}
//bez obliczen rownoleglych
def pierwsza(n:Int):Boolean = {
  if(n<2) return false
  return check(2,n)
}
//z obliczeniami rownoleglymi
def rpierwsza(n:Int):Boolean = {
  if(n<2) return false
  val (left,right) = parallel(check(2,(n-2)/2),check((n-2)/2,n))
  if(!left || !right) return true
  else return false
}
val arrTime = measure{
pierwsza(99999999)
}
val arrTime2 = measure{
rpierwsza(99999999)
}
//czas bez rownoleglych wychodzi okolo 0.67 ms, z rownoleglymi okolo 2-3 razy dluzszy (1,45ms) , czyli tego przykladu obliczen nie warto zrownolegniac

//problem 2 : suma liczb w duzej tablicy

def count(beg:Int, end:Int, array:Array[Int]):Int = {
  var i = beg
  var result: Int = 0
  while (i<end) {
    result = result + array(i)
    i = i + 1
  }
  return result
}

val arr = Array.fill[Int](10000000)(1)
val arr2 = Array.fill[Int](10000000)(1)
// bez obliczen rownoleglych
def suma(array:Array[Int]):Int = {
  return count(0,array.length,array)
}

// z obliczeniami rownoleglymi
def rsuma(array:Array[Int]):Int = {
  val (left,right) = parallel(count(0,array.length/2,array),count(array.length/2,array.length,array))
  return left+right
}
val arrTime3 = measure{
  suma(arr)
}
val arrTime4 = measure{
  suma(arr2)
}
//wyniki wychodzą różne, jednak czas w przypadku obliczen rownoleglych jest zazwyczaj okolo 2 razy szybszy ( ogolnie szybszy w przypadku tablicy o takim rozmiarze jest zawsze) pokazuje to ze w tym przypadku, w odroznieniu od poprzedniego przykladu oplaca sie rozlozyc obliczenia rownolegle.
