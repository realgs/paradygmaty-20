package main

import main.players.{SmartBot,RandomBot, Human}

object Main {
  def main(args: Array[String]): Unit = {
    val p1 = new Human("p1", 6, 6)
    val p2 = new RandomBot("p2", 6, 6)
    val game = new Game(p1, p2)

    game.start()
  }
}
