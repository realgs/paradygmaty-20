package Tests

import Tasks.Tree

object SameTreesTest {

  def areTreesTheSameTest(): Unit = {
    timeTest(10, 10)
    timeTest(10, 15)
    timeTest(10, 25)
  }

  private def timeTest(times: Int, depth: Int): Unit = {
    val tree = Tree.generateTree(depth, 1, 1000)
    val tree1 = Tree.copyTree(tree)
    var time = (0.toLong, 0.toLong )
    var currentTime = (0.toLong, 0.toLong)
    for(_ <- 1 to times)
    {
      currentTime = Utils.measureTime(Tree.areTreesTheSame(tree, tree1), Tree.areTreesTheSameParallel(tree, tree1))
      time = (time._1 + currentTime._1, time._2 + currentTime._2)
    }

    println("**********************************************************")
    println(times + " different runs of same trees method on tree with depth: " + depth)
    println("Average time for sequential same trees: " + (currentTime._1 / times))
    println("Average time for parallel same trees:   " + (currentTime._2 / times))
    println("**********************************************************\n")
  }
}
