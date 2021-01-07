package server

import java.io.IOException

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

object Game {
  def apply(): Game = new Game()
}

class Game() {
  private class TimeExceededException extends Exception
  private val TURN_TIME_LIMIT = 1000 * 30

  private var gameState = GameState()
  private val players: mutable.HashMap[String, Connection] = mutable.HashMap()
  private val connections: mutable.HashSet[Connection] = mutable.HashSet.empty

  def join(conn: Connection): Unit = connections.add(conn)

  private def arePlayersConnected: Boolean = players.forall { case (_, conn) => conn.isOpen }

  def run(): Future[Unit] = Future {
    try {
      while (connections.size < 2) {
        connections.foreach(conn => {
          conn.sendMessage(out => {
            out.writeByte(1)
            out.writeInt(1)
            out.writeUTF("Waiting for another player...")
          })
          Thread.sleep(2000)
        })
      }

      val itr = connections.iterator
      val p1 = itr.next()
      val p2 = itr.next()

      if (Random.nextDouble() < 0.5) {
        players.put("S", p1)
        players.put("N", p2)
      } else {
        players.put("S", p2)
        players.put("N", p1)
      }

      while (!GameState.isGameFinished(gameState) && arePlayersConnected) {
        players.foreach {
          case (side, conn) =>
            conn.sendMessage(out => {
              out.writeByte(3)
              val serializedState = GameState.serialize(gameState)
              out.writeInt(serializedState.length)
              out.writeBytes(serializedState)
              out.writeByte(1)
              out.writeInt(1)
              out.writeUTF(gameState.toString(side))
            })
        }

        val currentPlayer = gameState.nextTurn
        val conn = players(currentPlayer)
        var moved = false

        conn.sendMessage(out => {
          out.writeByte(2)
        })

        while (!moved) {
          try {
            val move = conn.getInput(TURN_TIME_LIMIT)
            if (move == -1) throw new TimeExceededException

            gameState = gameState.play(currentPlayer, move)
            moved = true
          } catch {
            case _: TimeExceededException =>
              conn.sendMessage(out => {
                out.writeByte(1)
                out.writeInt(1)
                out.writeUTF("Turn time limit exceeded")
              })
              gameState.skipTurn()
              moved = true
            case _: IllegalArgumentException =>
              conn.sendMessage(out => {
                out.writeByte(1)
                out.writeInt(1)
                out.writeUTF("Wrong input, pits are numbered 0 - 5 and you cannot choose empty pits")
                out.writeByte(2)
              })
          }
        }
      }

      if (arePlayersConnected) {
        players.foreach { case (side, conn) => conn.sendMessage(out => {
          out.writeByte(1)
          out.writeInt(1)
          out.writeUTF(gameState.toString(side, finished = true))
          out.writeByte(0)
        })
        }
      }
    } catch {
      case e: IOException =>
        e.printStackTrace()
        players.foreach { case (_, conn) => conn.sendMessage(out => {
          out.writeByte(1)
          out.writeInt(1)
          out.writeUTF("An opponent has disconnected")
        })
        }
    } finally {
      players.foreach { case (_, conn) => conn.playing = false }
    }
  }
}
