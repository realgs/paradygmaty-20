object Main {
  def main(args: Array[String]): Unit = {
    val p1 = new Player("p1",6,2)
    val p2 = new Player("p2",6,2)
    val game = new Game(p1,p2)

    game.start
  }
}
