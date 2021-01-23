import org.scalatest.FunSuite

class GameStateTest extends FunSuite {
  val gs = new GameState()

  test("moveState") {
    gs.move(2)(0)
  }
}
