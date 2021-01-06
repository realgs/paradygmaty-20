package server

import java.io.IOException
import java.net.{ServerSocket, Socket}

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global

object Server {
  private var port = 8888
  private var serverSocket: ServerSocket = _
  private var players = 0
  private val connections: mutable.HashSet[Connection] = mutable.HashSet.empty

  def numOfPlayers: Int = players

  def main(args: Array[String]): Unit = {
    if (!args.isEmpty) {
      port = Integer.parseInt(args(0))
    }

    try {
      serverSocket = new ServerSocket(port)
    } catch {
      case e: IOException =>
        e.printStackTrace()
        println(s"Cannot start server on port $port")
    }

    run()
  }

  private def run(): Unit = {
    try {
      while (true) {
        val client: Socket = serverSocket.accept()
        tryToAddConnection(client)
      }
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }

  private def tryToAddConnection(client: Socket): Unit = {
    val connection = Connection(serverSocket, client)
    if (players > 2) {
      connection.sendMessage(out => {
        out.writeByte(1)
        out.writeInt(1)
        out.writeUTF("Server is full at the moment, try again later")
        out.writeByte(0)
      })
    } else {
      players += 1
      println("A new player has connected")
      println(s"Current player count: $players")
      connections.add(connection)

      ClientListener(connection).listen().onComplete(_ => {
        connection.close()
        players -= 1
        println("A player has disconnected")
        println(s"Current player count: $players")
        connections.remove(connection)
      })
    }
  }
}
