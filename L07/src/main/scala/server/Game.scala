package server

import java.io.IOException

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

object Game {
  def apply(): Game = new Game()
}

class Game() {
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
              out.writeByte(1)
              out.writeInt(1)
              out.writeUTF(gameState.toString(side))
            })
        }

        players.get(gameState.nextTurn) match {
          case Some(conn) =>
            conn.sendMessage(out => {
              out.writeByte(2)
            })
            var moved = false
            while (!moved) {
              val move = conn.input.readInt()
              try {
                gameState = gameState.play(gameState.nextTurn, move)
                moved = true
              } catch {
                case _: IllegalArgumentException =>
                  conn.sendMessage(out => {
                    out.writeByte(1)
                    out.writeInt(1)
                    out.writeUTF("Wrong input, pits are numbered 0 - 5 and you cannot choose empty pits")
                    out.writeByte(2)
                  })
              }
            }
          case None => players.foreach { case (_, conn) => conn.close() }
        }
      }

      if (arePlayersConnected) {
        players.foreach { case (_, conn) => conn.sendMessage(out => {
          out.writeByte(1)
          out.writeInt(1)
          out.writeUTF(gameState.toString)
        })
        }

        val (sPoints, nPoints) = gameState.points
        val (winner, loser) = {
          if (sPoints > nPoints) (players("S"), players("N"))
          else if (sPoints < nPoints) (players("N"), players("S"))
          else (players("S"), players("S"))
        }

        if (winner.eq(loser)) {
          players.foreach { case (_, conn) => conn.sendMessage(out => {
            out.writeByte(1)
            out.writeInt(1)
            out.writeUTF("It's a draw!")
            out.writeByte(0)
          })
          }
        } else {
          winner.sendMessage(out => {
            out.writeByte(1)
            out.writeInt(1)
            out.writeUTF("You won")
            out.writeByte(0)
          })
          loser.sendMessage(out => {
            out.writeByte(1)
            out.writeInt(1)
            out.writeUTF("You lost")
            out.writeByte(0)
          })
        }
      }
    } catch {
      case e: IOException => e.printStackTrace()
    } finally {
      players.foreach { case (_, conn) => conn.playing = false }
    }
  }
}
