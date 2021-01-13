package main

import main.players.{RandomBot, SmartBot}
import akka.actor.{Actor, ActorSystem, Props}

object Main extends App {
    val p1 = new SmartBot("p1", 6, 6)
    val p2 = new RandomBot("p2", 6, 6)
    val game = new Game(p1, p2)

    game.start()

    Interface.printStartInfo()

    var currentPlayer = game.players(0)
    var currentPlayerIndex = 0

    Interface.drawBoard(game.players)
    while(game.started) {

        Interface.printTurn(currentPlayer)

        val chosenMove = currentPlayer.decideMove
        currentPlayerIndex = game.move(currentPlayerIndex, chosenMove)
        currentPlayer = game.players(currentPlayerIndex)
        Interface.drawBoard(game.players)
    }

    Interface.printFinish()
    Interface.printPoints(game.players)
    Interface.printWinner(game.winner)
}
