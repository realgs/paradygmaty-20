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