package server

import java.io.{DataInputStream, DataOutputStream}
import java.net.{ServerSocket, Socket}

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Connection {
  def apply(serverSocket: ServerSocket, socket: Socket): Connection = new Connection(serverSocket, socket)
}

class Connection (val serverSocket: ServerSocket, val socket: Socket) {
  private val input = new DataInputStream(socket.getInputStream)
  private val output = new DataOutputStream(socket.getOutputStream)
  private val buffer = mutable.Queue.empty[Int]

  private var open = true
  var playing = false
  var waitingForInput = false

  Future {
    while (open) {
      while (waitingForInput) {
        val in = input.readInt()
        if (waitingForInput) {
          buffer.enqueue(in)
          waitingForInput = false
        }
      }
      Thread.sleep(100)
    }
  }

  def isOpen: Boolean = open

  def sendMessage(message: DataOutputStream => Unit): Unit = {
    message(output)
    output.writeByte(127)
  }

  def close(): Unit = open = false

  def getInput(waitTime: Long): Int = {
    waitingForInput = true
    var exceeded = false
    val start = System.currentTimeMillis()

    while (!exceeded && waitingForInput && buffer.isEmpty) {
      exceeded = System.currentTimeMillis() - start >= waitTime && waitTime > 0
      Thread.sleep(100)
    }

    if (!exceeded && buffer.nonEmpty) buffer.dequeue()
    else {
      buffer.clear()
      -1
    }
  }
}
