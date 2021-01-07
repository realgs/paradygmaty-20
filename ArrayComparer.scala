package L6

// helper object made for testing
object ArrayComparer {
  def areArraysTheSame [T](arr1: Array[T], arr2: Array[T]): Boolean =
    if (arr1.length != arr2.length) false
    else if (arr1.length == 0) true
    else {
      for(i <- 0 until arr1.length) {
        if (arr1(i) != arr2(i)) return false
      }
      true
    }
}

