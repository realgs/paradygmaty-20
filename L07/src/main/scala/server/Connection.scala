package server

import java.io.{DataInputStream, DataOutputStream}
import java.net.{ServerSocket, Socket}

object Connection {
  def apply(serverSocket: ServerSocket, socket: Socket): Connection = new Connection(serverSocket, socket)
}

class Connection (val serverSocket: ServerSocket, val socket: Socket) {
  val input = new DataInputStream(socket.getInputStream)
  private val output = new DataOutputStream(socket.getOutputStream)
  private var open = true

  def isOpen: Boolean = open

  def sendMessage(message: DataOutputStream => Unit): Unit = {
    message(output)
    output.writeByte(127)
  }

  def close(): Unit = {
    open = false
  }
}
