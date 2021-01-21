package lab7

import scala.collection.parallel.CollectionConverters._

class MinMaxAlgorithm(val level: Int) extends Algorithm {
  override def move(position: Position): Int = {
    val node = new Node(position)
    val move = node.calculate(level, position.turn)
    move
  }

  class Node(val position: Position, val id: String = "start") {
    var highGround: Int = 0

    def calculate(depth: Int, algorithmColor: Player): Int = {
      if (depth <= 0) throw new IllegalArgumentException("depth must be positive")
      if (position.finished) position.modifiedHighground(algorithmColor)
      else {
        var max = -200
        var indexOfMaxHighGround = -1
        for (i <- (0 to 5).par) {
          if (position.holes(i + position.firstHole) != 0) {
            val eval = Node.calcPrun(position.next(i + 1), depth - 1, algorithmColor, -200, 200)
            if (eval > max) {
              max = eval
              indexOfMaxHighGround = i
            }
          }
        }
        highGround = max
        indexOfMaxHighGround + 1
      }
    }
  }

  object Node {
    def calcPrun(position: Position, depth: Int, algorithmColor: Player, _alfa: Int, _beta: Int): Int = {
      if (depth == 0 || position.finished) {
        position.modifiedHighground(algorithmColor)
      } else {
        var alfa = _alfa
        var beta = _beta
        var min = 200
        var max = -200

        var i = 0
        var flag = true
        while (i < 6 && flag) {
          if (position.holes(i + position.firstHole) != 0) {
            val eval = calcPrun(position.next(i + 1), depth - 1, algorithmColor, alfa, beta)
            if (position.turn == algorithmColor) {
              if (max < eval) max = eval
              if (alfa < eval) alfa = eval
            } else {
              if (min > eval) min = eval
              if (beta > eval) beta = eval
            }
            if (beta <= alfa) flag = false
          }
          i += 1
        }

        if (position.turn == algorithmColor) max
        else min
      }
    }
  }
}
