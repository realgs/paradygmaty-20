
object BubbleSort {

  def bubbleSort(array: Array[Double]): Array[Double] = {
    var swap = false

    for (i <- 0 until array.length - 1) {
      if (array(i + 1) < array(i)) {
        val temp = array(i)
        array(i) = array(i + 1)
        array(i + 1) = temp
        swap = true
      }
    }
    if(swap)
      bubbleSort(array)
      else
        array
  }

  def main(args: Array[String]): Unit = {
      var myList = Array(23.5, -54.63, 67.4, -533.544, 0, 563.41)
      bubbleSort(myList);
  }
}

