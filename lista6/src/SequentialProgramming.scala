import scala.annotation.tailrec

object SequentialProgramming {
  def mergeSort(arrayToSort: Array[Int]): Unit = {
    def merge(startIndex: Int, middleIndex: Int, endIndex: Int): Unit = {
      val tempArray = arrayToSort.slice(startIndex, endIndex + 1)

      var firstIndex = 0
      var secondIndex = middleIndex + 1 - startIndex
      var currentIndex = startIndex

      while (firstIndex + startIndex <= middleIndex && secondIndex + startIndex <= endIndex) {
        if (tempArray(firstIndex) < tempArray(secondIndex)) {
          arrayToSort(currentIndex) = tempArray(firstIndex)
          firstIndex = firstIndex + 1
        }
        else {
          arrayToSort(currentIndex) = tempArray(secondIndex)
          secondIndex = secondIndex + 1
        }
        currentIndex = currentIndex + 1
      }

      while (firstIndex + startIndex <= middleIndex) {
        arrayToSort(currentIndex) = tempArray(firstIndex)
        firstIndex = firstIndex + 1
        currentIndex = currentIndex + 1
      }

      while (secondIndex + startIndex <= endIndex) {
        arrayToSort(currentIndex) = tempArray(secondIndex)
        currentIndex = currentIndex + 1
        secondIndex = secondIndex + 1
      }

    }

    def sort(startIndex: Int, endIndex: Int): Unit = {
      if (startIndex < endIndex) {
        sort(startIndex, (startIndex + endIndex) / 2)
        sort((startIndex + endIndex) / 2 + 1, endIndex)
        merge(startIndex, (startIndex + endIndex) / 2, endIndex)
      }
    }

    sort(0, arrayToSort.length - 1)
  }

  def matrixProduct(firstMatrix: Array[Array[Int]], secondMatrix: Array[Array[Int]]): Array[Array[Int]] = {
    val multipliedMatrix = Array.ofDim[Int](firstMatrix.length, secondMatrix(0).length)

    for (i <- firstMatrix.indices) {
      for (j <- secondMatrix(0).indices) {
        for (k <- secondMatrix.indices) {
          multipliedMatrix(i)(j) = multipliedMatrix(i)(j) + firstMatrix(i)(k) * secondMatrix(k)(j)
        }
      }
    }

    multipliedMatrix
  }

  def longestCommonPrefix(words: Array[String]):String = {
    @tailrec
    def longestCommonPrefixHelper(first: String, second: String, result: StringBuilder):String = (first,second) match {
      case ("",_) | (_,"") => result.toString()
      case _ => if(first(0) == second(0)) longestCommonPrefixHelper(first.substring(1),second.substring(1),result.append(first(0))) else result.toString()
    }

    def commonPrefix(startingIndex: Int,endingIndex: Int): String = {
      if(startingIndex < endingIndex) {
        val leftLongestPrefix = commonPrefix(startingIndex,(startingIndex + endingIndex)/2)
        val rightLongestPrefix = commonPrefix((startingIndex + endingIndex)/2 + 1,endingIndex)
        longestCommonPrefixHelper(leftLongestPrefix,rightLongestPrefix,new StringBuilder(""))
      }
      else if(startingIndex == endingIndex) words(startingIndex)
      else ""
    }

    commonPrefix(0,words.length - 1)
  }
}
