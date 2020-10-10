def SelectSort(toSort:Array[Int]):Array[Int] =
{
  def getMaxValue(array:Array[Int]):Int=
    {
      var max = Integer.MIN_VALUE
      for(i <- array.indices)
      {
          if(max<array(i))
          {
            max = array(i)
          }
      }
      max
    }
  for(j <-toSort.length-1 to 0  by -1)
    {
      val max_val = getMaxValue(toSort.take(j+1))
      toSort(toSort.indexOf(max_val)) = toSort(j)
      toSort(j) = max_val
    }
  toSort
}

def InsertSort(toSort:Array[Int]):Array[Int] =
{
  for (i <- 1 until toSort.length)
  {
    val current = toSort(i)
    for (j <- i - 1 to 0 by -1)
    {
        if (current < toSort(j))
        {
          toSort(j+1) = toSort(j)
          toSort(j) = current
        }
    }
  }
  toSort
}

val array = Array(4, 2, 3, 1,5,5,-10,-2,1)
SelectSort(array)

val array2 = Array(4, 2, 3, 1,5,5,-10,-1,1)
InsertSort(array2)
