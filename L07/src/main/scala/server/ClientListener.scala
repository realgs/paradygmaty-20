package server

import java.io.IOException

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ClientListener {
  def apply(connection: Connection): ClientListener = new ClientListener(connection)
}

class ClientListener(val connection: Connection) {
  def listen(): Future[Unit] = Future {
    try {
      connection.sendMessage(out => {
        out.writeByte(1)
        out.writeInt(1)
        out.writeUTF("Connected successfully")
      })

      if (Server.numOfPlayers < 2) {
        val game = Server.startNewGame()
        game.join(connection)
        game.run()
      } else {
        val game = Server.getGame
        game.join(connection)
      }
      connection.playing = true

      while (connection.playing) {
        Thread.sleep(1000)
      }

    } catch {
      case e: IOException => e.printStackTrace()
    } finally {
      connection.close()
      connection.sendMessage(out => out.writeByte(0))
    }
  }
}
