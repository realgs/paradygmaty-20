val rand = scala.util.Random

def quicksort (array: Array[Int]): Array[Int] =
{
  if (array.length < 2) array
  else
  {
    val pivot = array(rand.nextInt(array.length))
    Array.concat(
      quicksort(array filter pivot.>),
      array filter pivot.==,
      quicksort(array filter pivot.<)
    )
  }
}

def insertsort(array:Array[Int]):Array[Int] =
{
  for (i <- 1 until array.length)
  {
    for (j <- i - 1 to 0 by -1)
    {
      if (array(j+1) < array(j))
      {
        var temp = array(j)
        array(j) = array(j+1)
        array(j+1) = temp
      }
    }
  }
  array
}

def randomTest(): Boolean = {
  var lista = new Array[Int](100)
  for(i <- 0.to(99)) lista(i) = rand.nextInt(100000) - 50000

  val insertList = insertsort(lista)
  val quickList = quicksort(lista)

  var flag1 = true
  for(i <- 1.to(99))
  {
    if (insertList(i) < insertList(i-1))
    {
      println("insertsort doesn't work")
      flag1 = false
    }
  }
  if(flag1) println("insertsort works")

  var flag2 = true
  for(i <- 1.to(99))
  {
    if (quickList(i) < quickList(i-1))
    {
      println("quicksort doesn't work")
      flag2 = false
    }
  }

  if(flag2) println("quicksort works")
  flag1 & flag2
}

for(i <- 1.to(10)) randomTest()