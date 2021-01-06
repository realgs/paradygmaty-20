package server

import java.io.IOException

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/*
 * Client's api
 * 0 - force disconnect
 * 1 - send message
 * 2 - waiting for input
 * 127 - eot
 */

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

      while (true) {
        while (Server.numOfPlayers != 2) {
          connection.sendMessage(out => {
            out.writeByte(1)
            out.writeInt(1)
            out.writeUTF("Waiting for another player...")
          })
          Thread.sleep(2000)
        }

        // Start game
      }
    } catch {
      case e: IOException => e.printStackTrace()
    } finally {
      connection.close()
    }
  }
}
