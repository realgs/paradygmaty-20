
class KalahGame(initial_stones: Int, pits_per_player: Int) {

  private val pits_amount = pits_per_player * 2 + 2
  private val engine = new GameEngine(
    new GameSettings(initial_stones, 1, pits_amount),
    (new HumanPlayer(1, pits_amount / 2, List.range(1, pits_amount / 2)),
    new HumanPlayer(2, pits_amount, List.range(pits_amount / 2 + 1, pits_amount))))

  def gameLoop(): Unit = {
    var last_status = GameStatus.IN_PROGRESS
    println("Game started:")
    engine.printState()

    while(last_status == GameStatus.IN_PROGRESS) {
      last_status = engine.playerMove()
      engine.printState()
    }

  }



}
