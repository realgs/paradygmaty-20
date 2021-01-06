package server

import org.scalatest.FunSuite

class GameStateTest extends FunSuite {
  test("Game logic") {
    var g = GameState()

    assert(g.nextTurn == "S")
    g = g.play("S", 0)
    assert(g.nextTurn == "S")
    assert(g.playerPits("S").sameElements(Array(0, 7, 7, 7, 7, 7)))
    g = g.play("S", 1)
    assert(g.nextTurn == "N")
    g = g.play("N", 1)
    assert(g.points == (2, 1))

    g = g.play("S", 5)
    g = g.play("N", 5)
    g = g.play("S", 5)
    g = g.play("S", 4)
    g = g.play("N", 5)
    g = g.play("N", 4)

    assert(g.points == (5, 4))
    assert(g.playerPits("S").sameElements(Array(5, 3, 10, 10, 1, 2)))
    assert(g.playerPits("N").sameElements(Array(11, 2, 9, 9, 0, 1)))

    g = g.play("S", 2)
    g = g.play("N", 2)
    g = g.play("S", 0)
    g = g.play("S", 1)
    g = g.play("S", 2)
    g = g.play("N", 3)

    assert(g.points == (8, 6))
    assert(g.playerPits("S").sameElements(Array(1, 1, 1, 16, 7, 8)))
    assert(g.playerPits("N").sameElements(Array(13, 4, 0, 0, 3, 4)))

    g = g.play("S", 5)
    g = g.play("N", 1)
    g = g.play("N", 4)
    g = g.play("S", 3)
    g = g.play("S", 0)
    g = g.play("N", 5)
    g = g.play("S", 5)
    g = g.play("N", 2)

    assert(g.points == (12, 11))
    assert(g.playerPits("S").sameElements(Array(0, 5, 5, 3, 11, 0)))
    assert(g.playerPits("N").sameElements(Array(17, 2, 0, 4, 2, 0)))

    g = g.play("S", 2)
    g = g.play("N", 4)
    g = g.play("N", 3)
    g = g.play("S", 5)
    g = g.play("S", 1)
    g = g.play("S", 5)
    g = g.play("S", 4)
    g = g.play("N", 0)
    g = g.play("N", 3)
    g = g.play("N", 1)

    assert(g.points == (21, 20))
    assert(g.playerPits("S").sameElements(Array(3, 2, 0, 7, 1, 2)))
    assert(g.playerPits("N").sameElements(Array(1, 0, 4, 0, 5, 6)))

    g = g.play("S", 4)
    g = g.play("N", 2)
    g = g.play("N", 5)
    g = g.play("S", 1)
    g = g.play("N", 0)
    g = g.play("S", 0)
    g = g.play("N", 4)
    g = g.play("S", 0)
    g = g.play("N", 3)
    g = g.play("S", 2)
    g = g.play("S", 4)
    g = g.play("S", 3)
    g = g.play("N", 0)

    assert(g.points == (26, 30))
    assert(g.playerPits("S").sameElements(Array(1, 1, 0, 0, 1, 7)))
    assert(g.playerPits("N").sameElements(Array(0, 2, 1, 0, 1, 2)))

    g = g.play("S", 0)
    g = g.play("N", 5)
    g = g.play("S", 5)
    g = g.play("N", 5)
    g = g.play("N", 4)
    g = g.play("N", 1)

    assert(g.points == (27, 36))
    assert(g.playerPits("S").sameElements(Array(1, 0, 0, 0, 1, 0)))
    assert(g.playerPits("N").sameElements(Array(1, 0, 3, 2, 0, 1)))

    g = g.play("S", 4)
    g = g.play("N", 5)
    g = g.play("N", 2)

    assert(GameState.isGameFinished(g))
    assert(g.points == (29, 43))
    assert(g.playerPits("S").sameElements(Array(0, 0, 0, 0, 0, 0)))
    assert(g.playerPits("N").sameElements(Array(0, 0, 0, 0, 0, 0)))
  }
}
