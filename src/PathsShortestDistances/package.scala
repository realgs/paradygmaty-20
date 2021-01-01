// Konrad Karanowski
import scala.collection.parallel.CollectionConverters._

/*
  Finding shortest paths between every two points in weighted graph (Floyd-Warshall algorithm).
  Graphs are represented as adjacency matrices. If vertex A and B are not connected with each other, distance is Int.MAX
 */

package object PathsShortestDistances
{
  def pathsShortestDistancesSequential(adjacencyMatrix: Array[Array[Int]]): Array[Array[Int]] = {
    val numVertex = adjacencyMatrix.length
    val shortestPaths = adjacencyMatrix.clone()
    for (i <- 0 until numVertex)
      for (j <- 0 until numVertex)
        for (k <- 0 until numVertex) {
          if (shortestPaths(i)(j) - shortestPaths(i)(k) > shortestPaths(k)(j)) {
            shortestPaths(i)(j) = shortestPaths(i)(k) + shortestPaths(k)(j)
            shortestPaths(j)(i) = shortestPaths(i)(k) + shortestPaths(k)(j)
          }
        }
    shortestPaths
  }

  def pathsShortestDistancesParallel(adjacencyMatrix: Array[Array[Int]]): Array[Array[Int]] = {
    val nJobs = Runtime.getRuntime.availableProcessors()
    val nJobs1 = nJobs / 2
    val nJobs2 = nJobs - nJobs1
    val numVertex = adjacencyMatrix.length
    val shortestPaths = adjacencyMatrix.clone()
    for (id1 <- (0 until nJobs1).par)
      for (i <- id1 * numVertex / nJobs1 until (id1 + 1) * numVertex / nJobs1)
        for (id2 <- (0 until nJobs2).par)
          for (j <- id2 * numVertex / nJobs2 until (id2 + 1) * numVertex / nJobs2)
            for (k <- 0 until numVertex) {
              if (shortestPaths(i)(j) - shortestPaths(i)(k) > shortestPaths(k)(j)) {
                val newShortest = shortestPaths(i)(k) + shortestPaths(k)(j)
                shortestPaths(i)(j) = newShortest
                shortestPaths(j)(i) = newShortest
              }
            }
    shortestPaths
  }
}
