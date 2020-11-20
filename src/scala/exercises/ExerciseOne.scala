package scala.exercises

import scala.trees.{BT, Node, Empty}

import scala.util.Random

object ExerciseOne {
  //Zad1 (3pkt)
  def generateTree(N: Int, start: Int, end: Int): BT[Int] = {
    def innerGenerator(N: Int, start: Int, end: Int): BT[Int] =
      if (N > 0) Node(start + new Random().nextInt(end - start + 1), generateTree(N - 1, start, end), generateTree(N - 1, start, end))
      else Empty
    if (start <= 0 || end <= 0) throw new IllegalArgumentException else innerGenerator(N, start, end)
  }
}
