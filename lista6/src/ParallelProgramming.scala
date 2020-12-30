import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object ParallelProgramming {
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
        if (endIndex - startIndex == arrayToSort.length - 1) {
          val leftFuture = Future {
            sort(startIndex, (startIndex + endIndex) / 2)
          }
          val rightFuture = Future {
            sort((startIndex + endIndex) / 2 + 1, endIndex)
          }
          Await.ready(leftFuture, Duration.Inf)
          Await.ready(rightFuture, Duration.Inf)
        }
        else {
          sort(startIndex, (startIndex + endIndex) / 2)
          sort((startIndex + endIndex) / 2 + 1, endIndex)
        }
        merge(startIndex, (startIndex + endIndex) / 2, endIndex)
      }
    }

    sort(0, arrayToSort.length - 1)
  }

  def matrixProduct(firstMatrix: Array[Array[Int]], secondMatrix: Array[Array[Int]]): Array[Array[Int]] = {
    val multipliedMatrix = Array.ofDim[Int](firstMatrix.length, secondMatrix(0).length)

    def computeRows(from: Int, to: Int): Unit = {
      for (i <- from until to) {
        for (j <- secondMatrix(0).indices) {
          multipliedMatrix(i)(j) = 0
          for (k <- secondMatrix.indices) {
            multipliedMatrix(i)(j) = multipliedMatrix(i)(j) + firstMatrix(i)(k) * secondMatrix(k)(j)
          }
        }
      }
    }

    val futures = new Array[Future[Unit]](4)

    for (i <- futures.indices) {
      futures(i) = Future {
        computeRows(i * firstMatrix.length / futures.length, (i + 1) * firstMatrix.length / futures.length)
      }
    }

    for (i <- futures.indices) {
      Await.ready(futures(i), Duration.Inf)
    }

    multipliedMatrix
  }
}
